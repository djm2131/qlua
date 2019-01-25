(* Reading interface for AFF files
 *  aff = affOpen[fileName] ; -- open an aff file and prepares it for access
 *  {type, data} = affGet(aff, keyPathString); -- extract data for a given key
 *  affClose[aff];   -- close the file and free the resources
 *)
affOpen[fileName_String] :=
  Module[{i, aff, affOpened, readHeader,
	  idString, dSize, dBase, dMant, dMaxExp, dMinExp, hSize,
	  dtOff, dtSize, dtCount,
	  stOff, stSize, stCount,
	  tbOff, tbSize, tbCount,
	  affSym, nodeTypes, affRoot, affNodes, affName, affOffset, affSize,
	  affType, affChildren,
	  affXGet, affXClose, affXFormat,
          name, type, offset, size, children},
    affOpened = False;
    aff = OpenRead[fileName, BinaryFormat -> True];

    If [aff == $Failed, Return[$Failed]];

    readHeader[f_InputStream] :=
      Module[{o,s,c,m},
	     o = BinaryRead[f,"UnsignedInteger64", ByteOrdering->1];
	     s = BinaryRead[f,"UnsignedInteger64", ByteOrdering->1];
	     c = BinaryRead[f,"UnsignedInteger64", ByteOrdering->1];
	     m = BinaryRead[f,"UnsignedInteger128", ByteOrdering->1];
	     {o,s,c}];

    idString = BinaryRead[aff, "TerminatedString"];
    dSize = BinaryRead[aff, "Byte"];
    dBase = BinaryRead[aff, "Byte"];
    dMant = BinaryRead[aff, "Byte"];
    dMaxexp = BinaryRead[aff, "UnsignedInteger16", ByteOrdering->1];
    dMinexp = BinaryRead[aff, "UnsignedInteger16", ByteOrdering->1];
    hSize = BinaryRead[aff, "UnsignedInteger32", ByteOrdering->1];
    {dtOff, dtSize, dtCount} = readHeader[aff];
    {stOff, stSize, stCount} = readHeader[aff];
    {tbOff, tbSize, tbCount} = readHeader[aff];

    (* read symbol table *)
    affSym = Table[False, {stCount}];
    SetStreamPosition[aff, stOff];
    For[i=1, i <= stCount, i++, affSym[[i]]=BinaryRead[aff,"TerminatedString"]];

    (* read nodes *)
    nodeTypes = {"void", "char", "int", "double", "complex"};
    affRoot = {};
    affNodes    = Table[False, {tbCount}];
    affName     = Table[False, {tbCount}];
    affOffset   = Table[False, {tbCount}];
    affSize     = Table[False, {tbCount}];
    affType     = Table[False, {tbCount}];
    affChildren = Table[{}, {tbCount}];

    SetStreamPosition[aff,tbOff];
    For[i = 1, i <= tbCount, i++,
	Module[{t,p,n,s,o},
	       t = nodeTypes[[BinaryRead[aff,"Byte"]]];
	       p = BinaryRead[aff, "UnsignedInteger64", ByteOrdering -> 1];
	       n = affSym[[BinaryRead[aff, "UnsignedInteger32",
				      ByteOrdering -> 1]+1]];
	       If [t == "void", s = 0; o = 0,
		     s = BinaryRead[aff, "UnsignedInteger32", ByteOrdering->1];
		     o = BinaryRead[aff, "UnsignedInteger64", ByteOrdering->1]];
	       affName[[i]] = n;
	       affOffset[[i]] = o;
	       affSize[[i]] = s;
	       affType[[i]] = t;
	       If [p == 0, affRoot = Append[affRoot,i],
		     affChildren[[p]] = Append[affChildren[[p]], i]]]];

    For[i = tbCount, i>= 1, i--,
	affNodes[[i]] = {name   -> affName[[i]],
			 type   -> affType[[i]],
			 offset -> affOffset[[i]],
			 size   -> affSize[[i]],
			 children -> Map[Function[c,
						  affName[[c]]->affNodes[[c]]],
					 affChildren[[i]]]}];

    root = Map[Function[c,affName[[c]]->affNodes[[c]]], affRoot];

    affXGet[path_String] :=
      Module[{t,o,s,d,fmt,lookup},
	     If [affOpened == False, Return[$Failed]];
	     lookup = Module[{ks, d, ch},
			     ks = StringSplit[path, "/"];
			     If [ks == {}, Return[$Failed]];
			     d = First[ks] /. root;
			     If [d == First[ks], Return[$Failed]];
			     For [ks = Rest[ks],
				     ks != {},
				     ch = children /. d;
				     If [ch == children, Return[$Failed]];
				     d = First[ks] /. ch;
				     ks = Rest[ks]];
			     If [ListQ[d], {type/.d, offset/.d, size/.d},
				      Return[$Failed]]];
	     If [lookup == $Failed, Return [$Failed]];
	     {t, o, s} = lookup;
	     fmt = Switch[t,
			  "void", Return[{t,0,{}}],
			  "char", "Character8",
			  "int",  "Integer32",
			  "double", "Real64",
			  "complex", "Complex128",
			  _, Return[$Failed]];
	     SetStreamPosition[aff, o];
	     d = BinaryReadList[aff, fmt, s, ByteOrdering -> 1];
	     {t, d}];

    affXClose[] := 
      Module[{},
	     If [affOpened == False, Return[]];
	     affOpened = False;
	     Close[aff]; aff =.;
	     i =. ;
	     root = .;
	     idString =. ;
	     dSize =. ;
	     dBase =. ;
	     dMant =. ;
	     dMaxexp =. ;
	     dMinexp =. ;
	     hSize =. ;
	     dtOff =. ; dtSize =. ; dtCount =. ;
	     stOff =. ; stSize =. ; stCount =. ;
	     tbOff =. ; tbSize =. ; tbCount =. ;
	     affSym =. ;
	     nodeTypes =. ;
	     affRoot =. ;
	     affNodes    =. ;
	     affName     =. ;
	     affOffset   =. ;
	     affSize     =. ;
	     affType     =. ;
	     affChildren =. ;
	     affXGet =.;
	     affXClose =. ;];
    affXFormat[] :=
      Module[{},
	     If [affOpened, StringJoin["affObject[\"", fileName, "\"]"],
			  "affObject[CLOSED]"]];
    affOpened = True;
    affObject[fileName, affXGet, affXClose, affXFormat]];

affGet[affObject[fn_,get_,cl_,fmt_], path_String] := get[path];
affClose[affObject[fn_,get_,cl_,fmt_]] := cl[];

(* Nice print format for affObjects *)
Format[affObject[fn_,a_,b_,fmt_]] := fmt[];
