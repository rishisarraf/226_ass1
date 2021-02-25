

fun parse file =            (* Function to convert file into a list of characters*)
let
    fun next_String input = (TextIO.inputAll input) 
    val stream = TextIO.openIn file
    val a = next_String stream
in
    explode(a)
end;

(* I have handled the cases of empty file and unevenFields in the input file. 
~ The function convertDelimiters assumes that the input file is well quoted, i.e, inputs invalid wrt quotes have NOT been handled.*)

exception emptyInputFile;
exception UnevenFields of string ;
exception lastRecordNotTerminatedByLF;
exception invalidQuotes;

fun convert(infile: char list , delim1: char, delim2: char)=     (* helper function to convert the input list of char into output list of char as per problem specification *)
	let 
		val output = ref nil;
		val len = length(infile);
		val i = ref 0;
		val current= ref #"a";
		val quote= #"\"";
		val comma= #",";
		val lf   = #"\n";
		val num_records = ref 0;
		val n = ref 0; 		(* stores the total number of fields in the first record *)
		val r = ref 0; 		(* stores the total number of fields seen in the current record *)
		val i = ref 0; 		(* stores the number of record corrently being looked at *)
		val j = ref 0; 		(* stores the variable for while loop *)
		val started_with_quote = ref false; (* stores bool, true if current field started with inverted comma*)
		val quote_open = ref false;
		val new_record = ref true;
		val new_field = ref true;
		val started_with_quotes = ref false; (* stores bool, true if the field being currently looked at started with quotes*)
		fun support(curr: char)= 
			let

				val curr_is_quote = curr=quote;
				val curr_is_delim1= curr=delim1;
				val curr_is_lf 	= curr=lf;

			in
				if !new_record then     (* if the curr char is the beginning of a new record*)
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
										(raise UnevenFields("Expected: "^Int.toString(!n)^" fields , Present: "^Int.toString(!r+1)^" fields on Line "^Int.toString(!i+1)^"\n"); 
										r := 0;
										i := !i + 1;
										new_record:=true;
										new_field:=true;
										curr::nil)
										(*new_record:=true;
										new_field:=true;
										curr::nil) *)
							else 
								(started_with_quotes:=false;
								quote_open := false;
								new_field:=false;
								new_record:=false;
								quote::curr::nil) 
				else 
					if !new_field then 				(* if the curr char is the beginning of a new field but not a new record*)
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
											(raise UnevenFields("Expected: "^Int.toString(!n)^" fields , Present: "^Int.toString(!r+1)^" fields on Line "^Int.toString(!i+1)^"\n");
											 r := 0;
											i := !i + 1;
											new_record:=true;
											new_field:=true;
											curr::nil)
											(*new_record:=true;
											new_field:=true;
											curr::nil) *)
								else 
									(started_with_quotes:=false;
									quote_open := false;
									new_field:=false;
									new_record:=false;
									quote::curr::nil)

					else 
						if !started_with_quotes then    (* if the field currently being looked at started with quotes*)
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
												if !i = 0 then
													(new_record:=true;
													i := !i + 1;
													n := !r + 1;
													r := 0;
													new_record:=true;
													new_field:=true;
													curr::nil)
												else													
													if !r + 1= !n then 
														(new_record:=true;
														i := !i + 1;
														r := 0;
														new_record:=true;
														new_field:=true;
														curr::nil)
													else 
														(raise UnevenFields("Expected: "^Int.toString(!n)^" fields , Present: "^Int.toString(!r+1)^" fields on Line "^Int.toString(!i+1)^"\n"); 
															new_record:=true;
															i := !i + 1;
															r := 0;
															new_record:=true;
															new_field:=true;
															curr::nil)
															(*new_record:=true;
															curr::nil)*) 
									else curr::nil 
						else
							if curr_is_quote then
								raise invalidQuotes
							else  
								if curr_is_delim1 then 
									(new_field:=true;
										r := !r + 1;
										quote::delim2::nil) 
								else 
									if curr_is_lf then
									 	if !i <> 0 then
											if !r + 1 <> !n then 
												(raise UnevenFields("Expected: "^Int.toString(!n)^" fields , Present: "^Int.toString(!r+1)^" fields on Line "^Int.toString(!i+1)^"\n");
												i := !i +1;
												r := 0;
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
		do (output := !output @ support(List.nth(infile,!j))handle UnevenFields(str) => (j := len; print(str)) ; j := !j + 1);   (* if unevenInput exception is handled, j is made len to stop further execution*)
		if len=0 then raise emptyInputFile else 
		(*print(Int.toString(!n)^" \n");*)
			if List.nth(infile,len-1)<> lf then raise lastRecordNotTerminatedByLF else
			!output 
	end;


fun convertDelimiters(infilename , delim1 , outfilename , delim2) =
	let 
		val vec = parse(infilename);
		val output_list = convert(vec, delim1, delim2); (*handle emptyInputFile=> nil;*)
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

