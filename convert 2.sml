fun parse file =
let
    fun next_String input = (TextIO.inputAll input) 
    val stream = TextIO.openIn file
    val a = next_String stream
in
    explode(a)
end;


exception emptyInputFile;
exception unevenInput;
exception unbalancedQuotes;

fun convert(infile: char list , delim1: char, delim2: char)=
	let 
		val output = ref nil;
		val len = length(infile);
		val i = ref 0;
		val current= ref #"a";
		val quote= #"\"";
		val comma= #",";
		val lf   = #"\n";
		val num_records = ref 0;
		val n = ref 0; (* stores the total number of fields in the first record *)
		val r = ref 0; (* stores the total number of fields seen in the current record *)
		val i = ref 0; (* stores the number of record corrently being looked at *)
		val j = ref 0; (* stores the variable for while loop *)
		val started_with_quote = ref false; (* stores bool, true if current field started with inverted comma*)
		val quote_open = ref false;
		val new_record = ref true;
		val new_field = ref true;
		val started_with_quotes = ref false;
		fun support(curr: char)= 
			let

				val curr_is_quote = curr=quote;
				val curr_is_delim1= curr=delim1;
				val curr_is_lf 	= curr=lf;

			in
				if !new_record then 
					if curr_is_quote then 
						(new_record:=false;
						 new_field:=false;
						 started_with_quotes:=true;
						 quote_open:=true;
						 curr::nil) 
					else 
						if curr_is_delim1 then 
							(new_field:=true;
							new_record:=false;
							 r := !r+1;
							 delim2::nil)
						else 
							if curr_is_lf then 
								if !i = 0 then
									(r := !r + 1; 
									n := !r;
									r := 0;
									i := !i + 1;
									new_record:=true;
									new_field:=true;
									curr::nil) 
								else 
									if !r + 1 = !n then 
										(r := 0;
										i := !i + 1;
										new_record:=true;
										new_field:=true;
										curr::nil)  
									else
										(print(Int.toString(80)^" "^Int.toString(!i)^" "^Int.toString(!r)^"\n"); 
										raise unevenInput)(*; 
										new_record:=true;
										new_field:=true;
										curr::nil) *)
							else 
								(started_with_quotes:=false;
								new_field:=false;
								new_record:=false;
								quote::curr::nil) 
				else 
					if !new_field then 
						if curr_is_quote then 
							(new_record:=false;
							 new_field:=false;
							 started_with_quotes:=true;
							 quote_open:=true;
							 curr::nil) 
						else 
							if curr_is_delim1 then 
								(new_field:=true;
								new_record:=false;
								 r := !r+1;
								 delim2::nil)
							else 
								if curr_is_lf then 
									if !i = 0 then
										(r := !r + 1; 
										n := !r;
										r := 0;
										i := !i + 1;
										new_record:=true;
										new_field:=true;
										curr::nil) 
									else 
										if !r + 1 = !n then 
											(r := 0;
											i := !i + 1;
											new_record:=true;
											new_field:=true;
											curr::nil)  
										else 
											(print(Int.toString(122)^" "^Int.toString(!i)^" "^Int.toString(!r)^"\n");
											raise unevenInput)(*; 
											new_record:=true;
											new_field:=true;
											curr::nil) *)
								else 
									(started_with_quotes:=false;
									new_field:=false;
									new_record:=false;
									quote::curr::nil)

					else 
						if !started_with_quotes then 
							if curr_is_quote then 
								if !quote_open then 
									(quote_open:=false;
										curr::nil) 
								else 
									(quote_open:=true;
										curr::nil) 
							else 
								if curr_is_delim1 then 
									if !quote_open then 
										curr::nil 
									else 
										(new_field:=true;
										r := !r + 1;
										delim2::nil) 
								else 
									if curr_is_lf then 
										if !quote_open then 
												curr::nil 
											else
												if !r + 1= !n then 
													(new_record:=true;
													i := !i + 1;
													r := 0;
													new_record:=true;
													new_field:=true;
													curr::nil)
												else
													(print(Int.toString(161)^" "^Int.toString(!i)^" "^Int.toString(!r)^"\n"); 
													raise unevenInput) 
														(*new_record:=true;
														curr::nil)*) 
									else curr::nil 
						else 
							if curr_is_delim1 then 
								(new_field:=true;
									r := !r + 1;
									quote::delim2::nil) 
							else 
								if curr_is_lf then
								 	if !i <> 0 then
										if !r + 1 <> !n then 
											(print(Int.toString(174)^" "^Int.toString(!i)^" "^Int.toString(!r)^"\n");
												raise unevenInput;
											new_record:=true;
											new_field:=true;
											quote::curr::nil)  
										else 
											  (	i := !i +1;
											  	r := 0;
											  	new_record:=true;
												new_field:=true;
												quote::curr::nil) 
									else 
										( n := !r + 1;
										i := !i + 1;
										r := 0;
										new_record:=true;
										new_field:=true;
										quote::curr::nil)
								else curr::nil
			end;
			
	in
		while !j<len 
		do (output := !output @ support(List.nth(infile,!j)); j := !j + 1);
		if len=0 then raise emptyInputFile else 
		!output
	end;
fun convertDelimiters(infilename , delim1 , outfilename , delim2) =
	let 
		val vec = parse(infilename);
		val output_list = convert(vec, delim1, delim2);
		val output_string = implode(output_list);
		val outStream = TextIO.openOut outfilename;
	in 
		TextIO.output(outStream, output_string)
	end;


fun csv2tsv ( infilename , outfilename ) =
	let
		val comma = #",";
		val tab   = #"\t";
	in 
		convertDelimiters( infilename, comma, outfilename, tab)
	end;


fun tsv2csv ( infilename , outfilename ) =
	let
		val comma = #",";
		val tab   = #"\t";
	in 
		convertDelimiters( infilename, tab, outfilename, comma)
	end;


(*convertDelimiters ("himym.csv", #",", "output_himym.ssv", #";");*)




(*val vec = parse("himym_uneven.csv");
val output_list=convert(vec, #",", #";");
val output_string= implode(output_list);
val outStream = TextIO.openOut "output_himym_uneven.txt";
TextIO.output(outStream,output_string );*)

