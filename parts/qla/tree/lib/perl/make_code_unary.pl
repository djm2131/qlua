######################################################################
# SciDAC Software Project
# BUILD_QLA Version 0.9
#
# make_code_unary.pl
#
# Author: C. DeTar
# Date:   09/13/02
######################################################################
#
# Top level code generation for unary operation
#
######################################################################
# Changes:
#
######################################################################
# Supporting files required:

use strict;

require("formatting.pl");
require("variable_names.pl");
require("expressions_scalar.pl");
require("expressions_tensor.pl");
require("expressions_gamma.pl");
require("datatypes.pl");
require("headers.pl");

use vars qw/ %def %dest_def %src1_def %src2_def /;
use vars qw/ @indent /;
use vars qw/ %unary_cmath %carith0 /;
use vars qw/ %precision_promotion /;
use vars qw/ $precision_double_abbrev $precision $temp_precision /;
use vars qw/ $var_i $var_x $var_x2 $var_global_sum /;
use vars qw/ $eqop_eq $eqop_peq /;
use vars qw/ $datatype_halffermion_abbrev /;

######################################################################

#---------------------------------------------------------------------
# Code for standard assignment (with operators)
#---------------------------------------------------------------------

sub make_code_unary {
  my($eqop,$qualifier) = @_;

  &print_top_matter($def{'declaration'},$var_i,$def{'dim_name'});
  #if($def{dim_name} ne "") {
  #  make_temp_ptr(*dest_def,$def{dest_name});
  #  make_temp_ptr(*src1_def,$def{src1_name});
  #}
  &print_val_eqop_op_val(\%dest_def,$eqop,\%src1_def,$qualifier);
  &print_end_matter($var_i,$def{'dim_name'});
}

#---------------------------------------------------------------------
# Code for traceless antihermitian part of colormatrix
#---------------------------------------------------------------------

sub make_code_antiherm_part {
    my($eqop) = @_;

    &print_top_matter($def{'declaration'},$var_i,$def{'dim_name'});
    &print_g_eqop_antiherm_g($eqop);
    &print_end_matter($var_i,$def{'dim_name'});
}

#---------------------------------------------------------------------
# Code for matrix function
#---------------------------------------------------------------------

sub make_code_matrix_func {
    my($eqop,$func) = @_;

    &print_top_matter($def{'declaration'},$var_i,$def{'dim_name'});
    if($def{'dim_name'} ne "") {
      &print_t_eqop_func_m($eqop,$func);
    } else {
    }
    &print_end_matter($var_i,$def{'dim_name'});
}

#---------------------------------------------------------------------
# Code for assignment from or to individual matrix or vector elements
#---------------------------------------------------------------------

sub make_code_getset_component {
    my($eqop,$ic,$is,$jc,$js,$qualifier) = @_;
    
    &print_top_matter($def{'declaration'},$var_i,$def{'dim_name'});
    &print_val_getset_component(\%dest_def,$eqop,\%src1_def,
				$ic,$is,$jc,$js,$qualifier);
    &print_end_matter($var_i,$def{'dim_name'});
}

#---------------------------------------------------------------------
# Code for elementary function of real or complex
#---------------------------------------------------------------------

# (These functions all take double precision arguments)
# changed to now use float version of function

sub make_code_unary_fcn {
    my($eqop,$unary_fcn) = @_;

    my($dest_value,$src1_value) = ($dest_def{'value'},$src1_def{'value'});
    my($temp_dest_type,$temp_src1_type,$macro,$math_name);

    my($rc_d,$rc_s1) = ($dest_def{'rc'},$src1_def{'rc'});

    &print_top_matter($def{'declaration'},$var_i,$def{'dim_name'});

    # Use macros for functions mapping complex -> real

    $macro = $carith0{$unary_fcn};
    if($rc_d eq "r" && $rc_s1 eq "c" && defined($macro)){
	print QLA_SRC @indent,"$dest_value = $macro($src1_value);\n";
    }
    else{
	# Type conversion for a complex result requires an intermediate
	#$temp_dest_type = &datatype_specific($dest_def{'t'},
	#				     $precision_double_abbrev);
	#if($rc_d eq "c" && $temp_dest_type ne $dest_def{'type'}){
	#    &print_def($temp_dest_type,$var_x2);
	#}
	# If type conversion to double is needed, define intermediate
	#$temp_src1_type = &datatype_specific($src1_def{'t'},
	#				     $precision_double_abbrev);
	#if($temp_src1_type ne $src1_def{'type'}){
	#    # (Can't be a register variable)
	#    print QLA_SRC @indent,"$temp_src1_type $var_x;\n";
	#    &print_s_eqop_s($rc_s1,$var_x,$eqop_eq,"",
	#		    $rc_s1,$src1_value,"");
	#    $src1_value = $var_x;
	#}

	# Pass pointers for complex arguments
	if($rc_s1 eq "c"){
	    $src1_value = "&($src1_value)";
	}
	
	# Use function name from table - defaults to given name
	$math_name = $unary_cmath{$unary_fcn};
	if(!defined($math_name)) { 
	  $math_name = $unary_fcn;
	  if($src1_def{'precision'} eq 'F') {
	    $math_name .= 'f';
	  }
	} else {
	  if($src1_def{'precision'} eq 'F') {
	    $math_name =~ s/QLA/QLA_F/;
	  } else {
	    $math_name =~ s/QLA/QLA_D/;
	  }
	}

	# Exception: build inline sign function (of reals)

	if($math_name =~ /sign/){
	    $src1_value = "$src1_value >= 0 ? 1. : -1.";
	}
	else{
	    $src1_value = "$math_name($src1_value)";
	}
	
	# Type conversion for a complex result uses the intermediate
	#if($rc_d eq "c" && $temp_dest_type ne $dest_def{'type'}){
	#    print QLA_SRC @indent,"$var_x2 = $src1_value;\n";
	#    &print_s_eqop_s($rc_d,$dest_value,$eqop_eq,"",
	#                    $rc_d,$var_x2,"");
	#}
	#else{
	print QLA_SRC @indent,"$dest_value = $src1_value;\n";
	#}
    }
    &print_end_matter($var_i,$def{'dim_name'});
}

#---------------------------------------------------------------------
# Code for Dirac spin projection and reconstruction
#---------------------------------------------------------------------

sub spproj_func {
  my($sign, $dir, $eqop) = @_;

  &open_siteloop($var_i,$def{'dim_name'});
#  &print_def_open_iter($var_i,$def{'dim_name'});
#  &print_align_indx();
  if($def{dim_name} ne "") {
    make_temp_ptr(*dest_def,$def{dest_name});
    make_temp_ptr(*src1_def,$def{src1_name});
  }
  my $ic = &get_row_color_index(*src1_def);
  &print_int_def($ic);
  my $maxic = $src1_def{'mc'};
  &open_iter($ic,$maxic);
  print_val_assign_spproj_dirs(\%dest_def, \%src1_def, $ic, $sign, $dir, $eqop);
  &close_iter($ic);
  if($def{'dim_name'} ne "") {
    &close_iter($var_i);
  }
}

sub sprecon_func {
  my($sign, $dir, $eqop) = @_;

  &open_siteloop($var_i,$def{'dim_name'});
#  &print_def_open_iter($var_i,$def{'dim_name'});
#  &print_align_indx();
  if($def{dim_name} ne "") {
    make_temp_ptr(*dest_def,$def{dest_name});
    make_temp_ptr(*src1_def,$def{src1_name});
  }
  my $ic = &get_row_color_index(*src1_def);
  &print_int_def($ic);
  my $maxic = $src1_def{'mc'};
  &open_iter($ic,$maxic);
  print_val_assign_sprecon_dirs(\%dest_def, \%src1_def, $ic, $sign, $dir, $eqop);
  &close_iter($ic);
  if($def{'dim_name'} ne "") {
    &close_iter($var_i);
  }
}

sub make_code_spproj_sprecon {
    my($eqop,$mu,$sign,$qualifier) = @_;

    &print_very_top_matter($def{'declaration'},$var_i,$def{'dim_name'});

    if($def{'qualifier'} eq "spproj") {
      &print_val_assign_spin($eqop, $mu, $sign, \&spproj_func);
    } else {
      &print_val_assign_spin($eqop, $mu, $sign, \&sprecon_func);
    }

    &print_very_end_matter($var_i,$def{'dim_name'});
}

#-----------------------------------------------------------------------
# Code for Wilson spin multiply
#-----------------------------------------------------------------------

sub wilsonspin_func {
  my($sign, $dir, $eqop) = @_;
  my %mytemp = ();

  &load_arg_hash(\%mytemp,'src1');
  $mytemp{t} = $datatype_halffermion_abbrev;
  $mytemp{type} = &datatype_specific($mytemp{t}, $temp_precision);
  $mytemp{value} = "t";
  $mytemp{precision} = $temp_precision;

  &open_siteloop($var_i,$def{'dim_name'});
#  &print_def_open_iter($var_i,$def{'dim_name'});
#  &print_align_indx();
  if($def{dim_name} ne "") {
    make_temp_ptr(*dest_def,$def{dest_name});
    make_temp_ptr(*src1_def,$def{src1_name});
  }
  my $ic = &get_row_color_index(*src1_def);
  &print_int_def($ic);
  my $maxic = $src1_def{'mc'};
  &open_iter($ic,$maxic);
  if($dir<4) {
    print_def($mytemp{type}, $mytemp{value});
    print_val_assign_spproj_dirs(\%mytemp, \%src1_def, $ic, $sign, $dir, "eq");
    print_val_assign_sprecon_dirs(\%dest_def, \%mytemp, $ic, $sign, $dir, $eqop);
  } else {
    print_val_assign_spproj_dirs(\%dest_def, \%src1_def, $ic, $sign, $dir, $eqop);
  }
  &close_iter($ic);
  if($def{'dim_name'} ne "") {
    &close_iter($var_i);
  }
}

sub make_code_wilsonspin {
  my($eqop,$mu,$sign) = @_;

  &print_very_top_matter($def{'declaration'},$var_i,$def{'dim_name'});
  &print_val_assign_spin($eqop, $mu, $sign, \&wilsonspin_func);
  &print_very_end_matter($var_i,$def{'dim_name'});
}

#---------------------------------------------------------------------
# Code for left or right multiplication by gamma matrix
#---------------------------------------------------------------------

sub mult_gamma_func {
  my($eqop, $leftright, $g) = @_;

  &open_siteloop($var_i,$def{'dim_name'});
#  &print_def_open_iter($var_i,$def{'dim_name'});
  if($def{dim_name} ne "") {
    make_temp_ptr(*dest_def,$def{dest_name});
    make_temp_ptr(*src1_def,$def{src1_name});
  }

  print_val_assign_gamma_times_dirs(*dest_def,*src1_def,$eqop,$leftright,$g);

  if($def{'dim_name'} ne "") {
    &close_iter($var_i);
  }
}

sub make_code_mult_gamma {
  my($eqop,$mu,$leftright) = @_;

  &print_very_top_matter($def{'declaration'},$var_i,$def{'dim_name'});
  &print_val_assign_gamma_times($eqop, $mu, $leftright, \&mult_gamma_func);
  &print_very_end_matter($var_i,$def{'dim_name'});
}

#---------------------------------------------------------------------
# Code for norm2 reduction or global sum
#---------------------------------------------------------------------

sub make_code_norm2_global_sum {
  my($eqop) = @_;
  $eqop eq 'eq' || die "only eq supported in reductions ($eqop)\n";

  my($global_type);
  my(%global_def) = %dest_def;
  my $dest_t = $dest_def{t};

  # The global variable inherits dest attributes, except for type and name
  # We accumulate global sums in the next higher precision relative to src1
  my($higher_precision) = $precision_promotion{$precision};
  $global_type = &datatype_specific($dest_t,$higher_precision);
  $global_def{'type'} = $global_type;
  $global_def{'value'} = $var_global_sum;
  $global_def{'precision'} = $higher_precision;

  &print_very_top_matter($def{'declaration'},$var_i,$def{'dim_name'});
#  &open_src_file;
#  &print_function_def($def{'declaration'});
  &print_nonregister_def($global_type,$var_global_sum);
  &print_fill(\%global_def,"zero");
  my $rdef;

  # Accumulate reduced result in global variable
  if($def{'qualifier'} eq "norm2"){
    # dest must be real in this case
    $rdef = &open_siteloop_reduce($var_i,$def{'dim_name'},\%global_def);
    &print_val_eqop_norm2_val($rdef,$eqop_peq,\%src1_def);
  }
  elsif($def{'qualifier'} eq "sum"){
    $rdef = &open_siteloop_reduce($var_i,$def{'dim_name'},\%global_def);
    if($def{dim_name} ne "") {
      make_temp_ptr(*src1_def,$def{src1_name});
    }
    &print_val_eqop_op_val($rdef,$eqop_peq,\%src1_def,"identity");
  }
  else{
    die "Can't do $def{'qualifier'}\n";
  }

  &close_siteloop_reduce($var_i,$def{'dim_name'},\%global_def,$rdef);

  # Assign reduced result to dest
  &print_val_eqop_op_val(\%dest_def,$eqop,\%global_def,"identity");

  &print_very_end_matter($var_i,$def{'dim_name'});
#  &close_brace();
#  &close_src_file;
}

#---------------------------------------------------------------------
# Build code for c eqop not a
#---------------------------------------------------------------------

sub make_code_boolean_not {
    my($eqop) = @_;

    &print_top_matter($def{'declaration'},$var_i,$def{'dim_name'});
    # Only integer operands supported here
    &print_s_eqop_s("r",$dest_def{'value'},$eqop,"","r",
		    "! $src1_def{'value'}");
    &print_end_matter($var_i,$def{'dim_name'});
}

1;
