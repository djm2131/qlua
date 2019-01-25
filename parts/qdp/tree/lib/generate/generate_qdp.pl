#!/usr/bin/perl
use strict;

my $maxnn = 1024;

# First read arguments and choose library and output directory

my $making_header_file = 0;
my $precision = '';
my $color = '';
my $lib = '';
my $outdir = '';

sub get_arg($\$) {
  my($r, $i) = @_;
  if(!$r) {
    if(!($ARGV[$$i+1]=~/^-/)) {
      $$i++;
      $r = $ARGV[$$i];
    }
  }
  return $r;
}

for(my $i=0; $i<=$#ARGV; $i++) {
  for($ARGV[$i]) {
    /^-h$/ && do { $making_header_file = 1; next; };
    /^-s$/ && do { $making_header_file = 0; next; };
    /^-p(.*)/ && do { $precision = get_arg($1, $i); next; };
    /^-c(.*)/ && do { $color = get_arg($1, $i); next; };
    /^-l(.*)/ && do {
      $lib = get_arg($1, $i);
      if($lib=~/([DFdf]*)([23Nn]*)/) {
	$precision = $1;
	$color = $2;
      }
      next;
    };
    /^-d(.*)/ && do { $outdir = get_arg($1, $i); next; };
    /.*/ && do {
      print "bad argument ", $_, "\n";
      print $0, " [-h|-s] [-p<precision>] [-c<color>] [-l<library>] [-d<dir>]\n";
      exit(1);
    };
  }
}

$precision = uc $precision;
$color = uc $color;

if( ! ($precision =~ /^$|^F$|^D$|^FD$|^DF$/) ) {
  print "bad precision ", $precision, "\n";
  exit(1);
}

if( ! ($color =~ /^$|^2$|^3$|^N$/) ) {
  print "bad color ", $color, "\n";
  exit(1);
}

my $pc = $precision.$color;
if($pc) { $pc = "_".$pc; }
$lib = $pc;
if(!$lib) { $lib = '_int'; }

$outdir =~ s/(.*)[\/]*/$1\//;

my $hlib = lc $lib;
if($precision eq "FD") { $hlib =~ s/fd/df/; }
if($making_header_file) {
  open HFILE, ">>".$outdir."qdp".$hlib.".h";
  my $qcolor = $color;
  if($qcolor eq "N") { $qcolor = "'N'"; }
  END {
    print HFILE <<"    EOF;";

#ifdef __cplusplus
}
#endif

#ifdef QDP_PROFILE
#include \"qdp${hlib}_profile.h\"
#endif

    EOF;

    if($pc =~ /^_[FD][23N]$/) {
      print HFILE <<"      EOF;";
  /* Translation to precision-generic names */
#if QDP_Precision == '$precision'
#include <qdp${hlib}_precision_generic.h>
#endif

      EOF;
    }

    if($pc =~ /[FD][23N]$/) {
      print HFILE <<"      EOF;";
  /* Translation to color-generic names */
#if QDP_Colors == $qcolor
#include <qdp${hlib}_color_generic.h>
#endif

      EOF;
    }

    if($pc =~ /^_[FD][23N]*$/) {
      my $clrs = "";
      if($color ne "") {
        $clrs = " && QDP_Colors == ".$qcolor;
      }
      print HFILE <<"      EOF;";
  /* Translation to fully generic names */
#if QDP_Precision == '$precision'$clrs
#include <qdp${hlib}_generic.h>
#endif

      EOF;
    }

    print HFILE "#endif // _QDP$pc";
    close HFILE;
  }
}

# Now define some variables

my(%datatypes);
my(@all_types, @shift_types, @float_types, @complex_types, @color_types, @arith_types);
my($thisdir);
($thisdir = $0) =~ s/[^\/]*$//;
scalar eval `cat ${thisdir}datatypes.pl`;

my @eqops  = (  "eq",  "peq",  "meq",  "eqm" );
my @veqops = ( "veq", "vpeq", "vmeq", "veqm" );

###########################
# define some subroutines #
###########################

# check if it is a valid type (if not blank)
sub check_type($) {
  my($t) = @_;
  if($t) {
    if(!$datatypes{$t}{ABBR}) {
      print "$0: error bad datatype $t\n"; 
      exit 1;
    }
  }
}

# check if the type arguments are compatible with the library we are making
sub correct_precision_and_color($$$) {
  my($t1, $t2, $t3) = @_;
  check_type($t1);
  check_type($t2);
  check_type($t3);
  my($has_precision) = 0;
  if( (($t1)&&(!$datatypes{$t1}{NO_PRECISION})) ||
      (($t2)&&(!$datatypes{$t2}{NO_PRECISION})) ||
      (($t3)&&(!$datatypes{$t3}{NO_PRECISION})) ) { $has_precision = 1; };
  my($has_color) = 0;
  if( (($t1)&&(!$datatypes{$t1}{NO_COLOR})) ||
      (($t2)&&(!$datatypes{$t2}{NO_COLOR})) ||
      (($t3)&&(!$datatypes{$t3}{NO_COLOR})) ) { $has_color = 1; };
  my($correct) = 1;
  if($precision) {
    if(!$has_precision) { $correct = 0; };
  } else {
    if($has_precision) { $correct = 0; };
  }
  if($color) {
    if(!$has_color) { $correct = 0; };
  } else {
    if($has_color) { $correct = 0; };
  }
  return $correct;
}

# check if an abbreviation is for a scalar (QLA) type
sub is_scalar($) {
  my($abbr) = @_;
  return $abbr =~ /^_[a-z]/;
}

sub is_double($) {
  my($var) = @_;
  return $var->{PC} =~ /^_D/;
}

sub qla_type($) {
  my($dt) = @_;
  my($type) = $dt->{TYPE};
  my($tpc) = $dt->{PC};
  my($name);
  $name = "QLA".$tpc."_".$type;
  return $name;
}

sub qla_ext_type($) {
  my($dt) = @_;
  my($type) = $dt->{TYPE};
  my($tpc) = $dt->{PC};
  $tpc =~ s/^_F/_D/;# ||
  #$tpc =~ s/^_D/_Q/;
  my($name);
  $name = "QLA".$tpc."_".$type;
  return $name;
}

sub prepare_dest($$) {
  my($sp0, $dest) = @_;
  my($abbr) = $dest->{ABBR};
  my($tpc) = $dest->{PC};
  my($r) = '';
  if($dest->{SCALAR}) {
    my $sp = $sp0;
    if($dest->{TEMP}) {
      if(!$dest->{VECT}) {
	my($ext) = qla_type($dest);
	if($dest->{EXTENDED}) {
	  $ext = qla_ext_type($dest);
	}
	if($dest->{MULTI}) {
	  if(($color eq 'N')&&(!$datatypes{$dest->{TYPE}}{NO_COLOR})) {
	    $r = $sp."typedef ".$ext."(nc,foo);\n";
	    #$r .= $sp."foo *dtemp = (foo *) malloc(ns*sizeof(foo));\n";
	    $r .= $sp."foo *dtemp=NULL;";
	    $r .= $sp."size_t nn = ns*sizeof(foo);\n";
	    $r .= $sp."if(nn>$maxnn) { dtemp = (foo *) malloc(nn); nn = 0; }\n";
	    $r .= $sp."foo dtempa[nn];\n";
	    $r .= $sp."if(nn) dtemp = dtempa;\n";
	  } else {
	    $r = $sp.$ext." *dtemp=NULL;\n";
	    #$r .= $sp."dtemp = ($ext *) malloc(ns*sizeof($ext));\n";
	    $r .= $sp."size_t nn = ns*sizeof($ext);\n";
	    $r .= $sp."if(nn>$maxnn) { dtemp = ($ext *) malloc(nn); nn = 0; }\n";
	    $r .= $sp."$ext dtempa[nn];\n";
	    $r .= $sp."if(nn) dtemp = dtempa;\n";
	  }
	} else {
	  if(($color eq 'N')&&(!$datatypes{$dest->{TYPE}}{NO_COLOR})) {
	    $r = $sp.$ext."(nc,dtemp);\n";
	  } else {
	    $r = $sp.$ext." dtemp;\n";
	  }
	}
	$r .= $sp."ONE {\n";
      }
    }
  } else {
    my $sp = $sp0 . "  ";
    if($dest->{VECT}) {
      $r .= $sp."QDP_prepare_dest(&dest[i]->dc);\n";
    } else {
      $r .= $sp."QDP_prepare_dest(&dest->dc);\n";
    }
  }
  return $r;
}

sub prepare_src($$$) {
  my($sp, $src, $v) = @_;
  $sp .= "  ";
  my($abbr) = $src->{ABBR};
  my($tpc) = $src->{PC};
  my($r) = '';
  if($abbr=~/^_[A-Z]/) { $r = $sp."QDP_prepare_src(&$v->dc);\n"; }
  return $r;
}

#sub prepare_fields($$$) {
#  my($dest, $src) = @_;
#  my($abbr) = $src->{ABBR};
#  my($tpc) = $src->{PC};
#  my($r) = '';
#  if($abbr=~/^_[A-Z]/) { $r = $sp."QDP_prepare_src(&$v->dc);\n"; }
#  return $r;
#}

sub global_sum($$) {
  my($dest, $eqop) = @_;
  my($r);
  my($abbr) = $dest->{ABBR};
  my($uabbr) = uc $abbr;
  my($tpc) = $dest->{PC};
  my($epc,$cpc) = {'',''};
  if(($tpc =~ /^_F/)&&$dest->{EXTENDED}) {
    ($epc = $tpc) =~ s/^_F/_D/;
    ($cpc = $tpc) =~ s/^_F/_FD/;
  } else {
    $epc = $tpc;
    $cpc = $tpc;
    #($epc = $tpc) =~ s/^_D/_Q/;
    #($cpc = $tpc) =~ s/^_D/_DQ/;
  }
  my($nc,$ncn) = ('','');
  my $st = "QLA".$epc."_".$dest->{TYPE};
  my $stp = "$st *";
  if(($color eq 'N')&&(!$datatypes{$dest->{TYPE}}{NO_COLOR})) {
    $nc = 'nc, ';
    $ncn = '_N';
    $stp = "$st(nc,(*))";
    $st .= "(nc,)";
  }
  my($rvar, $rexp, $rcp);
  if(!$eqop) { $eqop = "eq"; }
  if(($dest->{MULTI})||($dest->{VECT})) {
    my $vvar = "nv";
    if($dest->{MULTI}) { $vvar = "ns"; }
    if($dest->{TEMP}) {
      $rvar = "dtemp";
      $rexp = "QLA".$epc.$uabbr."_vp".$eqop.$uabbr."(".$nc.$rvar.", ($stp)REDUCE_GET(i), $vvar);";
      $rcp  = "QLA".$cpc.$uabbr."_v".$eqop.$uabbr."(".$nc."dest, ($stp)REDUCE_GET(0), $vvar);";
      if( $epc =~ /^_F/) {
	$r = "    QMP_sum_float_array((float *)dtemp, ($vvar)*(sizeof($st)/sizeof(float)));\n";
      } elsif( $epc =~ /^_D/) {
	$r = "    QMP_sum_double_array((double *)dtemp, ($vvar)*(sizeof($st)/sizeof(double)));\n";
      } else {
	$r = "    QDP".$ncn."_binary_reduce_multi(".$nc."QLA".$epc.$uabbr."_vpeq".$uabbr.", sizeof($st), dtemp, $vvar);\n";
      }
      $r .= "    TBARRIER;\n";
      $r .= "    QLA".$cpc.$uabbr."_v".$eqop.$uabbr."(".$nc."dest, dtemp, $vvar);\n";
      $r .= "    TBARRIER;\n";
      $r .= "    if(nn==0) free(dtemp);\n";
      $r .= "    if(nn==0) free(dtemp1);\n" if(($dest->{VECT})&&(!$dest->{MULTI}));
    } else {
      # shouldn't use this now
      $rvar = "dest";
      $rexp = "QLA".$epc.$uabbr."_vp".$eqop.$uabbr."(".$nc.$rvar.", ($stp)REDUCE_GET(i), $vvar);";
      if( $tpc =~ /^_F/) {
	$r = "    QMP_sum_float_array((float *)dest, ($vvar)*(sizeof($st)/sizeof(float)));\n";
      } elsif( $tpc =~ /^_D/) {
	$r = "    QMP_sum_double_array((double *)dest, ($vvar)*(sizeof($st)/sizeof(double)));\n";
      } else {
	$r = "    QDP".$ncn."_binary_reduce_multi(".$nc."QLA".$tpc.$uabbr."_vpeq".$uabbr.", sizeof($st), dest, $vvar);\n";
      }
      $r .= "    if(nn==0) free(dtemp1);\n" if(($dest->{VECT})&&(!$dest->{MULTI}));
    }
  } else {
    if($dest->{TEMP}) {
      $rvar = "&dtemp";
      if( ($dest->{TYPE} eq 'Real') && ($epc eq '_D') ) {
	$rexp = "dtemp += *($st *)REDUCE_GET(i);";
	$rcp  = "*dest = *($st *)REDUCE_GET(0);";
	$r  = "    QMP_sum_double(&dtemp);\n";
	$r .= "    TBARRIER;\n";
	$r .= "    *dest = dtemp;\n";
	$r .= "    TBARRIER;\n";
      } else {
	$rexp = "QLA".$epc.$uabbr."_p".$eqop.$uabbr."(".$nc.$rvar.", ($stp)REDUCE_GET(i));";
	$rcp  = "QLA".$cpc.$uabbr."_".$eqop.$uabbr."(".$nc."dest, ($stp)REDUCE_GET(0));";
	if( $epc =~ /^_F/) {
	  $r = "    QMP_sum_float_array((float *)&dtemp, sizeof($st)/sizeof(float));\n";
	} elsif( $epc =~ /^_D/) {
	  $r = "    QMP_sum_double_array((double *)&dtemp, sizeof($st)/sizeof(double));\n";
	} else {
	  $r = "    QDP".$ncn."_binary_reduce(".$nc."QLA".$epc.$uabbr."_peq".$uabbr.", sizeof($st), &dtemp);\n";
	}
	$r .= "    TBARRIER;\n";
	$r .= "    QLA".$cpc.$uabbr."_eq".$uabbr."(".$nc."dest, &dtemp);\n";
	$r .= "    TBARRIER;\n";
      }
    } else {
      # shouldn't use this now
      $rvar = "dest";
      $rexp = "QLA".$epc.$uabbr."_p".$eqop.$uabbr."(".$nc.$rvar.", ($stp)REDUCE_GET(i));";
      if( ($dest->{TYPE} eq 'Real') && ($tpc eq '_D') ) {
	$r = "    QMP_sum_double(dest);\n";
      } else {
	if( $tpc =~ /^_F/) {
	  $r = "  QMP_sum_float_array((float *)dest, sizeof($st)/sizeof(float));\n";
	} elsif( $tpc =~ /^_D/) {
	  $r = "  QMP_sum_double_array((double *)dest, sizeof($st)/sizeof(double));\n";
	} else {
	  $r = "  QDP".$ncn."_binary_reduce(".$nc."QLA".$tpc.$uabbr."_peq".$uabbr.", sizeof($st), dest);\n";
	}
      }
    }
  }

  if(!($eqop=~/^eq$/)) {
    die "unsupported eqop $eqop in reduction!\n";
  }
  #if($rvar=~/^dest$/) {
  #  die "unsupported rvar $rvar in reduction!\n";
  #}
  my $t1 = "";
  my $t2 = "";
  $t1 .= "  REDUCE_SET($rvar);\n";
  $t1 .= "  TBARRIER;\n";
  $t1 .= "  ONE {\n";
  $t1 .= "    for(int i=1; i<TSIZE; i++) {\n";
  $t1 .= "      $rexp\n";
  $t1 .= "    }\n";
  $t2 .= "  } else {\n";
  $t2 .= "    TBARRIER;\n";
  $t2 .= "    $rcp\n";
  $t2 .= "    TBARRIER;\n";
  $t2 .= "  }\n";

  return $t1.$r.$t2;
}

sub bod0($$$$$$$) {
  my($sp, $qla, $y0, $dv, $y1, $off, $arg) = @_;
  my($body) = $sp.$qla."( ".$y0.$dv.$y1.$arg." );\n";
  return $body;
}

sub bod1($$$$$$$$$$) {
  my($sp, $qla1, $qla2, $y0, $dv, $y1, $src, $y2, $off, $arg) = @_;
  my($body) = "";
  $body .= $sp."if( ".$src->{VAR}."->ptr ) {\n";
  #$body .= $sp."  "."/*QDP_math_time -= QDP_time();*/\n";
  #$body .= $sp."  ".$qla1."p".$qla2."( ".$y0.$dv.$y1.", ".$src->{VAR}."->ptr".$off.$y2.$arg." );\n";
  $body .= $sp."  ".$qla1."p".$qla2."( ".$y0.$dv.$y1.", QDP_offset_ptr(".$src->{VAR}.",".$off.")".$y2.$arg." );\n";
  #$body .= $sp."  "."/*QDP_math_time += QDP_time();*/\n";
  $body .= $sp."} else {\n";
  #$body .= $sp."  "."/*QDP_math_time -= QDP_time();*/\n";
  #$body .= $sp."  ".$qla1.$qla2."( ".$y0.$dv.$y1.", ".$src->{VAR}."->data".$off.$y2.$arg." );\n";
  $body .= $sp."  ".$qla1.$qla2."( ".$y0.$dv.$y1.", QDP_offset_data(".$src->{VAR}.",".$off.")".$y2.$arg." );\n";
  #$body .= $sp."  "."/*QDP_math_time += QDP_time();*/\n";
  $body .= $sp."}\n";
  return $body;
}

sub bod2($$$$$$$$$$$$$) {
  my($sp, $qla1, $qla2, $qla3, $y0, $dv, $y1, $src1, $y2, $src2, $y3, $off, $arg) = @_;
  my($body) = "";
  $body .= $sp."if( ".$src1->{VAR}."->ptr ) {\n";
  $body .= $sp."  if( ".$src2->{VAR}."->ptr ) {\n";
  #$body .= $sp."    "."/*QDP_math_time -= QDP_time();*/\n";
  $body .= $sp."    ".$qla1."p".$qla2."p".$qla3."( ".$y0.$dv.$y1.", QDP_offset_ptr(".$src1->{VAR}.",".$off.")".$y2.", QDP_offset_ptr(".$src2->{VAR}.",".$off.")".$y3.$arg." );\n";
  #$body .= $sp."    "."/*QDP_math_time += QDP_time();*/\n";
  $body .= $sp."  } else {\n";
  #$body .= $sp."    "."/*QDP_math_time -= QDP_time();*/\n";
  $body .= $sp."    ".$qla1."p".$qla2.$qla3."( ".$y0.$dv.$y1.", QDP_offset_ptr(".$src1->{VAR}.",".$off.")".$y2.", QDP_offset_data(".$src2->{VAR}.",".$off.")".$y3.$arg." );\n";
  #$body .= $sp."    "."/*QDP_math_time += QDP_time();*/\n";
  $body .= $sp."  }\n";
  $body .= $sp."} else {\n";
  $body .= $sp."  if( ".$src2->{VAR}."->ptr ) {\n";
  #$body .= $sp."    "."/*QDP_math_time -= QDP_time();*/\n";
  $body .= $sp."    ".$qla1.$qla2."p".$qla3."( ".$y0.$dv.$y1.", QDP_offset_data(".$src1->{VAR}.",".$off.")".$y2.", QDP_offset_ptr(".$src2->{VAR}.",".$off.")".$y3.$arg." );\n";
  #$body .= $sp."    "."/*QDP_math_time += QDP_time();*/\n";
  $body .= $sp."  } else {\n";
  #$body .= $sp."    "."/*QDP_math_time -= QDP_time();*/\n";
  $body .= $sp."    ".$qla1.$qla2.$qla3."( ".$y0.$dv.$y1.", QDP_offset_data(".$src1->{VAR}.",".$off.")".$y2.", QDP_offset_data(".$src2->{VAR}.",".$off.")".$y3.$arg." );\n";
  #$body .= $sp."    "."/*QDP_math_time += QDP_time();*/\n";
  $body .= $sp."  }\n";
  $body .= $sp."}\n";
  return $body;
}

sub qla_name($$$$$) {
  my($dest, $op, $src1, $func, $src2) = @_;
  my($tpc) = $pc;
  if($dest->{EXTENDED}) {
    $tpc =~ s/^_F/_DF/;# ||
    #$tpc =~ s/^_D/_QD/;
  }
  my($qla1, $qla2, $qla3, $qla4);
  $qla1 = "QLA".$tpc.$dest->{ABBR}."_";
  $qla2 = $op."_";
  $qla3 = $src1->{ABBR}.$src1->{ADJ}.$func;
  $qla3 =~ s/^_//;
  $qla4 = $src2->{ABBR}.$src2->{ADJ};
  $qla4 =~ s/^_//;
  if($qla4) { $qla3 .= '_'; }
  return ($qla1, $qla2, $qla3, $qla4);
}

sub get_nc_def($$$$) {
  my $s = '';
  my $sp = shift;
  if($color eq 'N') {
    my $t;
    for my $tt (@_) {
      if($tt->{TYPE} && !$tt->{SCALAR} && !$datatypes{$tt->{TYPE}}{NO_COLOR}) { $t = $tt; last; }
    }
    #if($t->{VECT}) {
    #  $s = "  int nc = QDP_get_nc(&".$t->{VAR}."[i]);\n";
    #} else {
      $s = $sp . "int nc = QDP_get_nc(".$t->{VAR}.");\n";
    #}
    my ($d) = @_;
    if($d->{SCALAR} && !$datatypes{$d->{TYPE}}{NO_COLOR}) {
      $s .= "  ".type_name($d)."(nc, (*dest)) = destv;\n";
    }
  }
  return $s;
}

# dest is scalar
sub func_body_scalar($$$$$$$$) {
  my($dest, $op, $src1, $func, $src2, $x1, $x2, $x3) = @_;

  my($dt) = $dest->{TYPE};
  my($s1t) = $src1->{TYPE};
  my($s2t) = $src2->{TYPE};

  if( ((!$s1t)||(!$datatypes{$s1t}{NO_PRECISION})) &&
      ((!$s2t)||(!$datatypes{$s2t}{NO_PRECISION})) ) {
    if(!is_double($dest)) {
      $dest->{EXTENDED} = 1;
    }
  }
  $dest->{TEMP} = 1;

  my($qla1, $qla2, $qla3, $qla4) = qla_name($dest, $op, $src1, $func, $src2);

  if($dest->{VECT}) {
    $x1 =~ s/(QLA_\S*\s+)[*]/$1&/g;
    $x2 =~ s/(QLA_\S*\s+)[*]/$1&/g;
    $x3 =~ s/(QLA_\S*\s+)[*]/$1&/g;
  }

  my($y0) = ('');
  if($color eq 'N') { $y0 = 'nc, '; }
  my($y1) = $x1;
  $y1 =~ s/[^, ]*[ *]+([^, *]+),/$1,/g;
  my($y2) = $x2;
  if($func eq '_func') {
    $y2 =~ s/\([^()]*\)([^()]*)$/$1/;
    $y2 =~ s/[()]*//g;
  }
  $y2 =~ s/[^, ]*[ *]+([^, *]+),/$1,/g;
  my($y3) = $x3;
  $y3 =~ s/[^, ]*[ *]+([^, *]+),/$1,/g;

  if($dest->{VECT}) {
    $dest->{VAR} .= "[i]";
    if($dest->{SCALAR}) { $dest->{VAR} = "\&".$dest->{VAR}; }
    $src1->{VAR} .= "[i]";
    if($src1->{SCALAR}) { $src1->{VAR} = "\&".$src1->{VAR}; }
    $src2->{VAR} .= "[i]";
    if($src2->{SCALAR}) { $src2->{VAR} = "\&".$src2->{VAR}; }
    $y1 =~ s/,/[i],/g;
    $y2 =~ s/,/[i],/g;
    $y3 =~ s/,/[i],/g;
  }

  if($y1) { $y1 = ", ".$y1; $y1 =~ s/[, ]*$//; }
  if($y2) { $y2 = ", ".$y2; $y2 =~ s/[, ]*$//; }
  if($y3) { $y3 = ", ".$y3; $y3 =~ s/[, ]*$//; }

  my($nsrc, $src);
  $nsrc = 2;
  if(!$s2t) {
    $nsrc--;
    $qla3 .= $qla4;
    $qla4 = "";
    $y2 .= $y3;
    $y3 = "";
    $src = $src1;
  }
  if(!$s1t) {
    $nsrc--;
    $qla2 .= $qla3;
    $qla3 = $qla4;
    $y1 .= $y2;
    $y2 = $y3;
    $src = $src2;
  } elsif($src1->{SCALAR}) {
    $nsrc--;
    $qla2 .= $qla3;
    $qla3 = $qla4;
    $y1 .= ", ".$src1->{VAR}.$y2;
    $y2 = $y3;
    $src = $src2;
  }

  my($subset) = "subset";
  if($dest->{MULTI}) { $subset = "subset[i]"; }

  #my($voff) = "+".$subset."->offset";
  #my($xoff) = "";
  my($xoff) = "0";
  #my($xarg) = ", ".$subset."->index, ".$subset."->len";
  my($xarg) = ", ".$subset."->index+toff, tlen";
  #my($voff) = $subset."->offset";
  my($voff) = $subset."->offset+toff";
  #my($varg) = ", ".$subset."->len";
  my($varg) = ", tlen";
  my($sp) = "  ";
  my($def) = "";
  my($top) = "";
  my($bot) = "";
  my($botsum) = "";
  my($global_eqop) = "eq";

  $def .= "  TGET;\n";
  $def .= get_nc_def("  ", $dest, $src1, $src2) if(!$dest->{VECT});

  if($dest->{VECT}) {
    if($dest->{MULTI}) {
      my($ssv) = "subset[i]";
      my($nvv) = "ns";
      #$def = "  int i;\n";
      if($dest->{TEMP}) {
	my($ext) = qla_type($dest);
	if($dest->{EXTENDED}) {
	  $ext = qla_ext_type($dest);
	}
	$def .= $sp.$ext." *dtemp=NULL;\n";
	#$def .= $sp.$ext." *dtemp = ($ext *) malloc(ns*sizeof($ext));\n";
	$def .= $sp."size_t nn = ns*sizeof($ext);\n";
	$def .= $sp."if(nn>$maxnn) { dtemp = ($ext *) malloc(nn); nn = 0; }\n";
	$def .= $sp."$ext dtempa[nn];\n";
	$def .= $sp."if(nn) dtemp = dtempa;\n";
      }
      $def .= "  ONE {\n";
      $def .= "    for(int i=0; i<ns; ++i) {\n";
      $top .= "    }\n";
      $top .= "  }\n";
      $top .= "  TBARRIER;\n";
      $top .= "\n";
      $top .= "  for(int i=0; i<$nvv; ++i) {\n";
      $top .= get_nc_def("  ", $dest, $src1, $src2);
      $bot  = "  }\n";
      $sp .= "  ";
    } else {
      my($ssv) = "subset";
      my($nvv) = "nv";
      my($tvar,$ttype);
      if($dest->{TEMP}) {
	my($ext) = qla_type($dest);
	if($dest->{EXTENDED}) {
	  $ext = qla_ext_type($dest);
	}
	$def .= $sp.$ext." *dtemp=NULL, *dtemp1=NULL;\n";
	#$def .= $sp.$ext." *dtemp = ($ext *) malloc(nv*sizeof($ext));\n";
	#$def .= $sp.$ext." *dtemp1 = ($ext *) malloc(nv*sizeof($ext));\n";
	$def .= $sp."size_t nn = nv*sizeof($ext);\n";
	$def .= $sp."if(nn>$maxnn) { dtemp = ($ext *) malloc(nn); dtemp1 = ($ext *) malloc(nn); nn = 0; }\n";
	$def .= $sp."$ext dtempa[nn], dtemp1a[nn];\n";
	$def .= $sp."if(nn) { dtemp = dtempa; dtemp1 = dtemp1a; }\n";
	$tvar = "dtemp";
	$ttype = $ext;
      } else {
	$ttype = qla_type($dest);
	#$def .= $sp.$ttype." *dtemp1 = ($ttype *) malloc(nv*sizeof($ttype));\n";
	$def .= $sp.$ttype." *dtemp1=NULL;\n";
	$def .= $sp."size_t nn = nv*sizeof($ttype);\n";
	$def .= $sp."if(nn>$maxnn) { dtemp1 = ($ttype *) malloc(nn); nn = 0; }\n";
	$def .= $sp."$ttype dtemp1a[nn];\n";
	$def .= $sp."if(nn) { dtemp1 = dtemp1a; }\n";
	$tvar = "dest";
      }
      ($global_eqop = $op) =~ s/_.*//;
      if(1) {
	my($dpc) = $ttype;
	$dpc =~ s/_[^_]*$//;
	$dpc .= uc $dest->{ABBR};
	$def .= $sp.$dpc."_veq_zero($tvar, $nvv);\n";
	$qla2 =~ s/_eqm_/_eq_/;
	$qla2 =~ s/_.eq_/_eq_/;
	$botsum = $dpc."_vpeq";
	$botsum .= uc $dest->{ABBR};
	$botsum .= "($tvar, dtemp1, $nvv);\n";
      } else {
	# need to strip _.* from $op
	if( ($op eq "eq") || ($op eq "eqm") ) {
	  my($dpc) = $ttype;
	  $dpc =~ s/_[^_]*$//;
	  $dpc .= uc $dest->{ABBR};
	  $def .= $sp.$dpc."_veq_zero(dtemp, $nvv);\n";
	  if($op eq "eq") {
	    $qla2 =~ s/eq/peq/;
	  } else {
	    $qla2 =~ s/eq/meq/;
	  }
	}
      }
      $def .= "  if(subset->len==0) for(int i=0; i<$nvv; ++i) {\n";
      $def .= prepare_dest("  ", $dest);
      $def .= "  }\n";
      $def .= "  int offset = 0;\n";
      $def .= "  int blen = QDP_block_size;\n";
      $def .= "  while(1) {\n";
      $def .= "    if( blen > $ssv->len - offset ) blen = $ssv->len - offset;\n";
      $def .= "    if( blen <= 0) break;\n";
      $def .= "    int toff, toff1; TSPLIT(toff, toff1, blen); int tlen = toff1-toff; toff += offset;\n";
      $def .= "    for(int i=0; i<$nvv; ++i) {\n";
      $def .= get_nc_def("    ", $dest, $src1, $src2);
      $def .= "      if(offset==0) {\n";
      $def .= "        ONE {\n";
      $top .= "        }\n";
      $top .= "        TBARRIER;\n";
      $top .= "      }\n";
      $bot  = "    }\n";
      $bot .= "    ".$botsum if($botsum);
      $bot .= "    offset += blen;\n";
      $bot .= "  }\n";
      $sp .= "    ";
    }
  } else { # not VECT
    if(!$dest->{TEMP}) { $def .= "  ONE {\n"; }
    $top .= "  }\n";
    $top .= "  TBARRIER;\n\n";
  }

  my($vdv) = "QDP_offset_data(".$dest->{VAR}.",".$voff.")";
  my($xdv) = "QDP_offset_data(".$dest->{VAR}.",".$xoff.")";
  if(($dest->{MULTI})&&(!$dest->{VECT})) {
    if($dest->{TEMP}) {
      $vdv = $xdv = "&dtemp[i]";
    } else {
      $vdv = $xdv = "&".$dest->{VAR}."[i]";
    }
    #$def .= "  int i;\n";
    $top .= "  for(int i=0; i<ns; i++) {\n";
    $bot = "  }\n";
    #$sp .= "  ";
  } elsif($dest->{VECT}) {
    if($dest->{MULTI}) {
      if($dest->{TEMP}) {
	$vdv = $xdv = "&dtemp[i]";
      } else {
	$vdv = $xdv = $dest->{VAR};
      }
    } else {
      $vdv = $xdv = "&dtemp1[i]";
    }
  } else {
    if($dest->{TEMP}) {
      $vdv = $xdv = "&dtemp";
    } else {
      $vdv = $xdv = $dest->{VAR};
    }
  }
  $bot .= global_sum($dest, $global_eqop);
  my($sp2) = $sp."  ";

  if(!$dest->{VECT} || $dest->{MULTI}) {
    $top .= "  int toff, toff1; TSPLIT(toff, toff1, ".$subset."->len); int tlen = toff1-toff;\n";
  }

  my($body);
  if($nsrc==0) {
    $body = "{\n";
    $body .= $def;
    $body .= prepare_dest($sp, $dest);
    $body .= $top;
    $body .= $sp."if( $subset->indexed ) {\n";
    $body .= bod0($sp2, $qla1."x".$qla2, $y0, $xdv, $y1, $xoff, $xarg);
    $body .= $sp."} else {\n";
    $body .= bod0($sp2, $qla1."v".$qla2, $y0, $vdv, $y1, $voff, $varg);
    $body .= $sp."}\n";
    $body .= $bot;
    $body .= "}\n";
  } elsif($nsrc==1) {
    if($datatypes{$src->{TYPE}}{NO_SHIFT}) {
      my($xt) = $y1.", ".$src->{VAR}."->data".$xoff.$y2;
      my($vt) = $y1.", ".$src->{VAR}."->data".$voff.$y2;
      $body = "{\n";
      $body .= $def;
      $body .= prepare_dest($sp, $dest);
      $body .= prepare_src($sp, $src, $src->{VAR});
      $body .= $top;
      $body .= $sp."if( $subset->indexed ) {\n";
      $body .= bod0($sp2, $qla1."x".$qla2.$qla3, $y0, $xdv, $xt, $xoff, $xarg);
      $body .= $sp."} else {\n";
      $body .= bod0($sp2, $qla1."v".$qla2.$qla3, $y0, $vdv, $vt, $voff, $varg);
      $body .= $sp."}\n";
      $body .= $bot;
      $body .= "}\n";
    } else {
      $body = "{\n";
      $body .= $def;
      $body .= prepare_dest($sp, $dest);
      $body .= prepare_src($sp, $src, $src->{VAR});
      $body .= $top;
      $body .= $sp."if( $subset->indexed ) {\n";
      $body .= bod1($sp2, $qla1."x".$qla2, $qla3, $y0, $xdv, $y1, $src, $y2, $xoff, $xarg);
      $body .= $sp."} else {\n";
      $body .= bod1($sp2, $qla1."v".$qla2, $qla3, $y0, $vdv, $y1, $src, $y2, $voff, $varg);
      $body .= $sp."}\n";
      $body .= $bot;
      $body .= "}\n";
    }
  } else {
    $body = "{\n";
    $body .= $def;
    $body .= prepare_dest($sp, $dest);
    $body .= prepare_src($sp, $src1, $src1->{VAR});
    $body .= prepare_src($sp, $src2, $src2->{VAR});
    $body .= $top;
    $body .= $sp."if( $subset->indexed ) {\n";
    $body .= bod2($sp2, $qla1."x".$qla2, $qla3, $qla4, $y0, $xdv, $y1, $src1, $y2, $src2, $y3, $xoff, $xarg);
    $body .= $sp."} else {\n";
    $body .= bod2($sp2, $qla1."v".$qla2, $qla3, $qla4, $y0, $vdv, $y1, $src1, $y2, $src2, $y3, $voff, $varg);
    $body .= $sp."}\n";
    $body .= $bot;
    $body .= "}\n";
  }
  return $body;
}

# dest is field
sub func_body_field($$$$$$$$) {
  my($dest, $op, $src1, $func, $src2, $x1, $x2, $x3) = @_;

  my($dt) = $dest->{TYPE};
  my($s1t) = $src1->{TYPE};
  my($s2t) = $src2->{TYPE};

  my($qla1, $qla2, $qla3, $qla4) = qla_name($dest, $op, $src1, $func, $src2);

  if($dest->{VECT}) {
    $x1 =~ s/(QLA_\S*\s+)[*]/$1&/g;
    $x2 =~ s/(QLA_\S*\s+)[*]/$1&/g;
    $x3 =~ s/(QLA_\S*\s+)[*]/$1&/g;
  }

  my($y0) = ('');
  if($color eq 'N') { $y0 = 'nc, '; }
  my($y1) = $x1;
  $y1 =~ s/[^, ]*[ *]+([^, *]+),/$1,/g;
  my($y2) = $x2;
  if($func eq '_func') {
    $y2 =~ s/\([^()]*\)([^()]*)$/$1/;
    $y2 =~ s/[()]*//g;
  }
  $y2 =~ s/[^, ]*[ *]+([^, *]+),/$1,/g;
  my($y3) = $x3;
  $y3 =~ s/[^, ]*[ *]+([^, *]+),/$1,/g;

  if($dest->{VECT}) {
    $dest->{VAR} .= "[i]";
    if($dest->{SCALAR}) { $dest->{VAR} = "\&".$dest->{VAR}; }
    $src1->{VAR} .= "[i]";
    if($src1->{SCALAR}) { $src1->{VAR} = "\&".$src1->{VAR}; }
    $src2->{VAR} .= "[i]";
    if($src2->{SCALAR}) { $src2->{VAR} = "\&".$src2->{VAR}; }
    $y1 =~ s/,/[i],/g;
    $y2 =~ s/,/[i],/g;
    $y3 =~ s/,/[i],/g;
  }

  if($y1) { $y1 = ", ".$y1; $y1 =~ s/[, ]*$//; }
  if($y2) { $y2 = ", ".$y2; $y2 =~ s/[, ]*$//; }
  if($y3) { $y3 = ", ".$y3; $y3 =~ s/[, ]*$//; }

  my($nsrc, $src);
  $nsrc = 2;
  if(!$s2t) {
    $nsrc--;
    $qla3 .= $qla4;
    $qla4 = "";
    $y2 .= $y3;
    $y3 = "";
    $src = $src1;
  }
  if(!$s1t) {
    $nsrc--;
    $qla2 .= $qla3;
    $qla3 = $qla4;
    $y1 .= $y2;
    $y2 = $y3;
    $src = $src2;
  } elsif($src1->{SCALAR}) {
    $nsrc--;
    $qla2 .= $qla3;
    $qla3 = $qla4;
    $y1 .= ", ".$src1->{VAR}.$y2;
    $y2 = $y3;
    $src = $src2;
  }

  my($subset) = "subset";

  my($xoff) = "0";
  my($xarg) = ", ".$subset."->index+toff, tlen";
  my($voff) = $subset."->offset+toff";
  my($varg) = ", tlen";
  my($sp) = "  ";
  my($def) = "";
  my($top) = "";
  my($bot) = "";
  my($botsum) = "";
  my($global_eqop) = "eq";

  $def .= "  TGET;\n";
  $def .= get_nc_def("  ", $dest, $src1, $src2) if(!$dest->{VECT});

  if($dest->{VECT}) {
    my($ssv) = "subset";
    my($nvv) = "nv";
    $def .= "  if(subset->len==0) for(int i=0; i<$nvv; ++i) {\n";
    $def .= prepare_dest("  ", $dest);
    $def .= "  }\n";
    $def .= "  int offset = 0;\n";
    $def .= "  int blen = QDP_block_size;\n";
    $def .= "  while(1) {\n";
    $def .= "    if( blen > $ssv->len - offset ) blen = $ssv->len - offset;\n";
    $def .= "    if( blen <= 0) break;\n";
    $def .= "    int toff, toff1; TSPLIT(toff, toff1, blen); int tlen = toff1-toff; toff += offset;\n";
    $def .= "    for(int i=0; i<$nvv; ++i) {\n";
    $def .= get_nc_def("    ", $dest, $src1, $src2);
    $def .= "      if(offset==0) {\n";
    $def .= "        ONE {\n";
    $top .= "        }\n";
    $top .= "        TBARRIER;\n";
    $top .= "      }\n";
    $bot  = "    }\n";
    $bot .= "    ".$botsum if($botsum);
    $bot .= "    offset += blen;\n";
    $bot .= "  }\n";
    $sp .= "    ";
  } else { # not VECT
    if(!$dest->{TEMP}) { $def .= "  ONE {\n"; }
    $top .= "  }\n";
    $top .= "  TBARRIER;\n\n";
  }

  #my($vdv) = $dest->{VAR}."->data".$voff;
  #my($xdv) = $dest->{VAR}."->data".$xoff;
  my($vdv) = "QDP_offset_data(".$dest->{VAR}.",".$voff.")";
  my($xdv) = "QDP_offset_data(".$dest->{VAR}.",".$xoff.")";
  my($sp2) = $sp."  ";

  if(!$dest->{VECT}) {
    $top .= "  int toff, toff1; TSPLIT(toff, toff1, ".$subset."->len); int tlen = toff1-toff;\n";
  }

  my($body);
  if($nsrc==0) {
    $body = "{\n";
    $body .= $def;
    $body .= prepare_dest($sp, $dest);
    $body .= $top;
    $body .= $sp."if( $subset->indexed ) {\n";
    $body .= bod0($sp2, $qla1."x".$qla2, $y0, $xdv, $y1, $xoff, $xarg);
    $body .= $sp."} else {\n";
    $body .= bod0($sp2, $qla1."v".$qla2, $y0, $vdv, $y1, $voff, $varg);
    $body .= $sp."}\n";
    $body .= $bot;
    $body .= "}\n";
  } elsif($nsrc==1) {
    if($datatypes{$src->{TYPE}}{NO_SHIFT}) {
      my($xt) = $y1.", ".$src->{VAR}."->data".$xoff.$y2;
      my($vt) = $y1.", ".$src->{VAR}."->data".$voff.$y2;
      $body = "{\n";
      $body .= $def;
      $body .= prepare_dest($sp, $dest);
      $body .= prepare_src($sp, $src, $src->{VAR});
      $body .= $top;
      $body .= $sp."if( $subset->indexed ) {\n";
      $body .= bod0($sp2, $qla1."x".$qla2.$qla3, $y0, $xdv, $xt, $xoff, $xarg);
      $body .= $sp."} else {\n";
      $body .= bod0($sp2, $qla1."v".$qla2.$qla3, $y0, $vdv, $vt, $voff, $varg);
      $body .= $sp."}\n";
      $body .= $bot;
      $body .= "}\n";
    } else {
      $body = "{\n";
      $body .= $def;
      $body .= prepare_dest($sp, $dest);
      $body .= prepare_src($sp, $src, $src->{VAR});
      $body .= $top;
      $body .= $sp."if( $subset->indexed ) {\n";
      $body .= bod1($sp2, $qla1."x".$qla2, $qla3, $y0, $xdv, $y1, $src, $y2, $xoff, $xarg);
      $body .= $sp."} else {\n";
      $body .= bod1($sp2, $qla1."v".$qla2, $qla3, $y0, $vdv, $y1, $src, $y2, $voff, $varg);
      $body .= $sp."}\n";
      $body .= $bot;
      $body .= "}\n";
    }
  } else {
    $body = "{\n";
    $body .= $def;
    $body .= prepare_dest($sp, $dest);
    $body .= prepare_src($sp, $src1, $src1->{VAR});
    $body .= prepare_src($sp, $src2, $src2->{VAR});
    $body .= $top;
    $body .= $sp."if( $subset->indexed ) {\n";
    $body .= bod2($sp2, $qla1."x".$qla2, $qla3, $qla4, $y0, $xdv, $y1, $src1, $y2, $src2, $y3, $xoff, $xarg);
    $body .= $sp."} else {\n";
    $body .= bod2($sp2, $qla1."v".$qla2, $qla3, $qla4, $y0, $vdv, $y1, $src1, $y2, $src2, $y3, $voff, $varg);
    $body .= $sp."}\n";
    $body .= $bot;
    $body .= "}\n";
  }
  return $body;
}

# construct body of function
sub func_body($$$$$$$$) {
  my($dest, $op, $src1, $func, $src2, $x1, $x2, $x3) = @_;

  if($dest->{SCALAR}) {
    return func_body_scalar($dest, $op, $src1, $func, $src2, $x1, $x2, $x3);
  } else {
    return func_body_field($dest, $op, $src1, $func, $src2, $x1, $x2, $x3);
  }
}

# get full name for type
sub type_name($) {
  my($typehash) = @_;
  my($type) = $typehash->{TYPE};
  my($scalar) = $typehash->{SCALAR};
  my($p);
  if($precision eq 'FD') {
    if($typehash->{DEST}) { $p = "F"; }
    else { $p = "D"; }
  } elsif($precision eq 'DF') {
    if($typehash->{DEST}) { $p = "D"; }
    else { $p = "F"; }
  } else {
    $p = $precision;
  }
  my($tpc);
  if($datatypes{$type}{NO_PRECISION}) {
    $tpc = '';
  } elsif($datatypes{$type}{NO_COLOR}) {
    $tpc = '_'.$p;
  } else {
    $tpc = '_'.$p.$color;
  }
  $typehash->{PC} = $tpc;
  my($name);
  if($scalar) {
    $name = "QLA".$tpc."_".$type;
  } else {
    $name = "QDP".$tpc."_".$type;
  }
  return $name;
}

# get abbreviation for type
sub type_abbr($) {
  my($t) = @_;
  my($abbr);
  if($t->{TYPE}) {
    $abbr = "_".$datatypes{$t->{TYPE}}{ABBR};
    if($t->{SCALAR}) { $abbr = lc $abbr; }
  }
  return $abbr;
}

# construct name of QDP function
sub qdp_name($$$$$$) {
  my($dest, $op, $src1, $func, $src2, $mangle) = @_;

  my($da) = $dest->{ABBR};
  if(($mangle)&&($dest->{ABBR}=~/[a-z]/)) { $da .= "1"; }
  my($s1a) = $src1->{ABBR};
  if(($mangle)&&($src1->{ABBR}=~/[a-z]/)) { $s1a .= "1"; }
  my($s2a) = $src2->{ABBR};
  if(($mangle)&&($src2->{ABBR}=~/[a-z]/)) { $s2a .= "1"; }

  my($s1aa) = $s1a.$src1->{ADJ};
  my($s2aa) = $s2a.$src2->{ADJ};
  my($multi) = "";
  if($dest->{MULTI}) { $multi = "_multi"; }
  return "QDP".$pc.$da."_".$op.$s1aa.$func.$s2aa.$multi;
}

sub get_arg_v($) {
    my ($t) = @_;
    my $s = '';
    if($t->{SCALAR}) {
	my $type = $t->{TYPE};
	if($color ne 'N' || $datatypes{$type}{NO_COLOR}) {
	    $s = type_name($t)." ".$t->{VAR}."[], ";
	} else {
	  if($t->{VAR} eq 'dest') {
	    $s = "void *".$t->{VAR}."v, ";
	  } else {
	    $s = "void *".$t->{VAR}.", ";
	  }
	}
    } else {
	$s = type_name($t)." *".$t->{VAR}."[], ";
    }
    return $s;
}

sub get_arg_s($) {
    my ($t) = @_;
    my $s = '';
    if($t->{SCALAR}) {
	my $type = $t->{TYPE};
	if($color ne 'N' || $datatypes{$type}{NO_COLOR}) {
	    $s = type_name($t)." *".$t->{VAR}.", ";
	} else {
	  if($t->{VAR} eq 'dest') {
	    $s = "void *".$t->{VAR}."v, ";
	  } else {
	    $s = "void *".$t->{VAR}.", ";
	  }
	}
    } else {
	$s = type_name($t)." *".$t->{VAR}.", ";
    }
    return $s;
}

# construct arguments to QDP function
sub qdp_args($$$$$$) {
  my($dest, $x1, $src1, $x2, $src2, $x3) = @_;
  if($dest->{VECT}) {
    $x1 =~ s/,/[],/g;
    $x2 =~ s/,/[],/g;
    $x3 =~ s/,/[],/g;
    $x1 =~ s/(QLA_\S*\s+)[*]/$1/g;
    $x2 =~ s/(QLA_\S*\s+)[*]/$1/g;
    $x3 =~ s/(QLA_\S*\s+)[*]/$1/g;
  }
  my($s1t) = $src1->{TYPE};
  my($s2t) = $src2->{TYPE};
  $dest->{VAR} = "dest";
  if($s1t) {
    if($s2t) {
      $src1->{VAR} = 'src1';
      $src2->{VAR} = 'src2';
    } else {
      $src1->{VAR} = 'src';
      $src2->{VAR} = '';
    }
  } else {
    $src1->{VAR} = '';
    if($s2t) {
      $src2->{VAR} = 'src';
    } else {
      $src2->{VAR} = '';
    }
  }
  my($s1arg) = '';
  if($s1t) {
    if($dest->{VECT}) {
      $s1arg = get_arg_v($src1);
    } else {
      $s1arg = get_arg_s($src1);
    }
  }
  my($s2arg) = '';
  if($s2t) {
    if($dest->{VECT}) {
      $s2arg = get_arg_v($src2);
    } else {
      $s2arg = get_arg_s($src2);
    }
  }
  my($args) = "( ";
  #if($color eq 'N') { $args .= 'int nc, '; }
  #$args .= type_name($dest);
  if(($dest->{MULTI})||($dest->{VECT})) {
      $args .= get_arg_v($dest);
  } else {
      $args .= get_arg_s($dest);
  }
  $args .= $x1.$s1arg.$x2.$s2arg.$x3;
  if($dest->{MULTI}) {
    $args .= "QDP_Subset subset[], int ns )";
  } elsif($dest->{VECT}) {
    $args .= "QDP_Subset subset, int nv )";
  } else {
    $args .= "QDP_Subset subset )";
  }
  return $args;
}

###########################
# comment making routines #
###########################

my($comment0, $comment1, $comment2);

sub comment0($) {
  ($comment0) = @_;
}

sub comment1($) {
  ($comment1) = @_;
}

sub comment2($) {
  ($comment2) = @_;
}

sub get_h_comment() {
  my($c) = '';
  if($comment0) {
    $c .= "\n";
    $c .= "/*\n";
    $c .= " *  ".$comment0."\n";
    $c .= " */\n";
    $c .= "\n" if(!$comment1);
    $comment0 = "";
  }
  if($comment1) {
    $c .= "\n";
    $c .= "/*\n";
    $c .= " *  ".$comment1."\n";
    $c .= " */\n";
    $c .= "\n";
    $comment1 = "";
  }
  return $c;
}

sub get_c_comment() {
  my($c) = '';
  $c .= "/*\n";
  if($comment0) {
    $c .= " *  ".$comment0."\n";
    $c .= " *\n" if($comment1);
  }
  if($comment1) {
    $c .= " *  ".$comment1."\n";
  }
  $c .= " */\n";
  $c .= "\n";
  return $c;
}

# write out prototype or source
sub write_function(\%$\%$\%\%) {
  my($dest, $op, $src1, $func, $src2, $xargs) = @_;

  my($dt) = $dest->{TYPE};
  my($s1t) = $src1->{TYPE};
  my($s2t) = $src2->{TYPE};

  if( correct_precision_and_color($dt, $s1t, $s2t) ) {

    $dest->{ABBR} = type_abbr($dest);
    $src1->{ABBR} = type_abbr($src1);
    $src2->{ABBR} = type_abbr($src2);

    my($qladest) = type_name($dest);
    $qladest =~ s/QDP/QLA/;
    my($x1) = $xargs->{PROT1};
    $x1 =~ s/QLAPCTYPE/$qladest/;
    my($x2) = $xargs->{PROT2};
    $x2 =~ s/QLAPCTYPE/$qladest/;
    my($x3) = $xargs->{PROT3};
    $x3 =~ s/QLAPCTYPE/$qladest/;

    if($func) { $func = "_".$func; }

    my($name) = qdp_name($dest, $op, $src1, $func, $src2, 0);
    my($mangled_name) = qdp_name($dest, $op, $src1, $func, $src2, 1);
    if($op =~ /^v(.*)/) {
      $op = $1;
      $dest->{VECT} = 1;
    }
    my($args) = qdp_args($dest, $x1, $src1, $x2, $src2, $x3);

    if($making_header_file) {
      my($out) = "void ".$name.$args.";\n";
      print HFILE get_h_comment();
      print HFILE $out;
    } else {
      my($head) = "void\n".$name.$args."\n";
      my($body) = func_body($dest, $op, $src1, $func, $src2, $x1, $x2, $x3);
      open CFILE, '>'.$outdir.$mangled_name.".c";
      print CFILE get_c_comment();
      print CFILE "#include \"qdp".$hlib."_internal.h\"\n\n";
      print CFILE $head.$body;
      close CFILE;
    }
  }
}

# create functions from definitions
sub make_functions(\%) {
  my($arg) = @_;

  if($arg->{DO_FD}) {
    if($precision eq 'DF') { $precision = 'FD'; }
    if($precision ne 'FD') { return; }
  } elsif($arg->{DO_DF}) {
    if($precision eq 'FD') { $precision = 'DF'; }
    if($precision ne 'DF') { return; }
  } elsif(($precision eq 'FD')||($precision eq 'DF')) {
    return;
  }
  $pc = $precision.$color;
  if($pc) { $pc = "_".$pc; }

  my(%xargs);
  $xargs{PROT1} = $arg->{EXTRA_ARGS1};
  $xargs{PROT2} = $arg->{EXTRA_ARGS2};
  $xargs{PROT3} = $arg->{EXTRA_ARGS3};

  my(@funcs);
  if($arg->{'FUNCS'}) { @funcs = @{$arg->{'FUNCS'}}; }
  if(!@funcs) { @funcs = ( '' ); }
  for my $func (@funcs) {

    for my $op (@{$arg->{'EQ_OPS'}}) {

      for my $dt (@{$arg->{'DEST_TYPES'}}) {

	my(@src1_types);
	if($arg->{'SRC1_TYPES'}) { @src1_types = @{$arg->{'SRC1_TYPES'}}; }
	if(!@src1_types) { @src1_types = ( '' ); }
	if($src1_types[0] eq 'DEST') { @src1_types = ( "$dt" ); }
	for my $s1t (@src1_types) {

	  my(@src1_adjs);
	  if( ($arg->{SRC1_DO_ADJ}) && ($s1t ne '') &&
	      (!$datatypes{$s1t}{NO_ADJ}) ) { @src1_adjs = ( '', 'a' ); }
	  elsif( ($arg->{SRC1_SQ_ADJ}) && ($s1t ne '') &&
		 (!$datatypes{$s1t}{NO_ADJ}) &&
		 ($datatypes{$s1t}{SQUARE}) ) {@src1_adjs=('','a');}
	  elsif( ($arg->{SRC1_ADJ}) && ($s1t ne '') &&
		 (!$datatypes{$s1t}{NO_ADJ}) ) { @src1_adjs = ( 'a' ); }
	  else { @src1_adjs = ( '' ); }
	  for my $s1adj (@src1_adjs) {

	    my(@src2_types);
	    if($arg->{'SRC2_TYPES'}) { @src2_types = @{$arg->{'SRC2_TYPES'}}; }
	    if(!@src2_types) { @src2_types = ( '' ); }
	    if($src2_types[0] eq 'DEST') { @src2_types = ( "$dt" ); }
	    if($src2_types[0] eq 'SRC1') { @src2_types = ( "$s1t" ); }
	    for my $s2t (@src2_types) {

	      my(@src2_adjs);
	      if( ($arg->{SRC2_DO_ADJ}) && ($s2t ne '') &&
		  (!$datatypes{$s2t}{NO_ADJ}) ) {@src2_adjs=('','a');}
	      elsif( ($arg->{SRC2_SQ_ADJ}) && ($s2t ne '') &&
		     (!$datatypes{$s2t}{NO_ADJ}) &&
		     ($datatypes{$s2t}{SQUARE}) ) {@src2_adjs=('','a');}
	      elsif( ($arg->{SRC2_ADJ}) && ($s2t ne '') &&
		     (!$datatypes{$s2t}{NO_ADJ}) ) {@src2_adjs=('a');}
	      else { @src2_adjs = ( '' ); }
	      for my $s2adj (@src2_adjs) {

		my %dest = ( TYPE=>$dt, DEST=>1 );
		if($arg->{DEST_SCALAR}) { $dest{SCALAR} = 1; }
		if($arg->{DEST_MULTI}) { $dest{MULTI} = 1; }

		my %src1 = ( TYPE=>$s1t, SRC1=>1 );
		if($arg->{SRC1_SCALAR}) { $src1{SCALAR} = 1; }
		#if($s1adj) { $src1{ADJ} = 1; }
		$src1{ADJ} = $s1adj;

		my %src2 = ( TYPE=>$s2t, SRC2=>1 );
		if($arg->{SRC2_SCALAR}) { $src2{SCALAR} = 1; }
		#if($s2adj) { $src2{ADJ} = 1; }
		$src2{ADJ} = $s2adj;

		write_function(%dest,$op,%src1,$func,%src2,%xargs);
	      } # src2 adj
	    } # src2
	  }  # src1 adj
	}  # src1
      }  # dest
    }  # eqop
  }  # func
}

# Last we actually create the functions

scalar eval `cat ${thisdir}functions.pl`;
