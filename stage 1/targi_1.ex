

include std/filesys.e -- needed for walk_dir
include std/wildcard.e -- needed for is_match
include std/console.e	-- needed for prompt_string	
include std/sequence.e  -- for split
include std/text.e -- for switch
with trace
-- address to use:
-- C:\Euphoria\bin\programs

constant IN = 0, OUT = 1, FALSE = 0, TRUE = 1, EOF = -1
integer ok
sequence answer, filenameTmp
object exit_code
atom counter = 0

--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

procedure popToD(integer fn_asm)
		printf(fn_asm, "%s", {"@SP\nM=M-1\nA=M\nD=M\n"})
end procedure

procedure popToA(integer fn_asm)
		printf(fn_asm, "%s", {"@SP\nM=M-1\nA=M\nA=M\n"})
end procedure
--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- D=M+D
procedure popFunc1(integer fn_asm, object offset, object segment)	
	printf(fn_asm, "%s", {"@" & offset & "\nD=A\n@" & segment & "\nD=M+D\n@13\nM=D\n"})
	popToD(fn_asm)
	printf(fn_asm, "%s", {"@13\nA=M\nM=D\n"})
end procedure

-- D=A+D
procedure popFunc2(integer fn_asm, object offset, object segment)	
	printf(fn_asm, "%s", {"@" & offset & "\nD=A\n@" & segment & "\nD=A+D\n@13\nM=D\n"})
	popToD(fn_asm)
	printf(fn_asm, "%s", {"@13\nA=M\nM=D\n"})
end procedure
--~~~~~~~~~~~~~~~~
procedure popFunc(sequence splitLine, integer fn_asm)

object segment

	switch splitLine[2] do
	
		case "local" then
			segment = "LCL"
			popFunc1(fn_asm, splitLine[3], segment)
			
		case "argument" then
			segment = "ARG"
			popFunc1(fn_asm, splitLine[3], segment)
			
		case "this" then
			segment = "THIS "
			popFunc1(fn_asm, splitLine[3], segment)
			
		case "that" then
			segment = "THAT "
			popFunc1(fn_asm, splitLine[3], segment)
			
		case "temp" then
			segment = "5"
			popFunc2(fn_asm, splitLine[3], segment)		
			
		case "pointer" then
			segment = "THIS"
			popFunc2(fn_asm, splitLine[3], segment)
			
		case "static" then
			segment = filenameTmp[1] & "." & splitLine[3]			
			popToD(fn_asm)
			printf(fn_asm, "%s", {"@" & segment & "\nM=D\n"})
			
		case else
			printf(fn_asm, "%s", {"ERROR"})
	end switch
			
end procedure

--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A=M+D
procedure pushFunc1(integer fn_asm, object offset, object segment)	
	printf(fn_asm, "%s", {"@" & offset & "\nD=A\n@" & segment & "\nA=M+D\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"})
end procedure

-- A=A+D
procedure pushFunc2(integer fn_asm, object offset, object segment)	
	printf(fn_asm, "%s", {"@" & offset & "\nD=A\n@" & segment & "\nA=A+D\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"})
end procedure
--~~~~~~~~~~~~~~~~~~~
procedure pushFunc(sequence splitLine, integer fn_asm)

object segment

	switch splitLine[2] do
	
		case "local" then
			segment = "LCL"
			pushFunc1(fn_asm, splitLine[3], segment)
			
		case "argument" then
			segment = "ARG"
			pushFunc1(fn_asm, splitLine[3], segment)
			
		case "this" then
			segment = "THIS"
			pushFunc1(fn_asm, splitLine[3], segment)
			
		case "that" then
			segment = "THAT"
			pushFunc1(fn_asm, splitLine[3], segment)
			
		case "temp" then
			segment = "5"
			pushFunc2(fn_asm, splitLine[3], segment)		
			
		case "pointer" then
			segment = "THIS"
			pushFunc2(fn_asm, splitLine[3], segment)
			
		case "static" then
			segment = filenameTmp[1] & "." & splitLine[3]			
			printf(fn_asm, "%s", {"@" & segment & "\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"})
			
		case "constant" then
			printf(fn_asm, "%s", {"@"& splitLine[3] &"\nD=A\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"})
			
		case else
			printf(fn_asm, "%s", {"ERROR"})
	end switch
	
end procedure

--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

procedure addFunc(sequence splitLine, integer fn_asm)
	popToD(fn_asm)
	printf(fn_asm, "%s", {"@SP\nA=M-1\nM=M+D\n"})
end procedure

procedure subFunc(sequence splitLine, integer fn_asm)
	popToD(fn_asm)
	printf(fn_asm, "%s", {"@SP\nA=M-1\nM=M-D\n"})
end procedure

procedure negFunc(sequence splitLine, integer fn_asm)
	popToD(fn_asm)
	printf(fn_asm, "%s", {"@SP\nA=M\nM=-D\n@SP\nM=M+1\n"})
end procedure

procedure eqFunc(sequence splitLine, integer fn_asm)
--	trace(1)
	popToD(fn_asm)
	popToA(fn_asm)
	printf(fn_asm, "%s", {"D=D-A\n@IF_TRUE"})
	printf(fn_asm, "%d" , {counter})
	printf(fn_asm, "%s", {"\nD; JEQ\n@SP\nA=M\nM=0\n@end"})
	printf(fn_asm, "%d" , {counter+1})
	printf(fn_asm, "%s", {"\n0; JMP\n(IF_TRUE"})
	printf(fn_asm, "%d" , {counter})
	printf(fn_asm, "%s", {")\n@SP\nA=M\nM=-1\n(end"})
	printf(fn_asm, "%d" , {counter+1})
	printf(fn_asm, "%s", {")\n@SP\nM=M+1\n"})
	counter = counter + 2
--	trace(0)
	end procedure

procedure gtFunc(sequence splitLine, integer fn_asm)
	popToD(fn_asm)
	popToA(fn_asm)
	printf(fn_asm, "%s", {"D=A-D\n@IF_TRUE"})	--printf(fn_asm, "%s", {"D=D-A\n@IF_TRUE"})
	printf(fn_asm, "%d" , {counter})
	printf(fn_asm, "%s", {"\nD; JGT\n@SP\nA=M\nM=0\n@end"})
	printf(fn_asm, "%d" , {counter+1})
	printf(fn_asm, "%s", {"\n0; JMP\n(IF_TRUE"})
	printf(fn_asm, "%d" , {counter})
	printf(fn_asm, "%s", {")\n@SP\nA=M\nM=-1\n(end"})
	printf(fn_asm, "%d" , {counter+1})
	printf(fn_asm, "%s", {")\n@SP\nM=M+1\n"})
	counter = counter + 2
end procedure

procedure ltFunc(sequence splitLine, integer fn_asm)
	popToD(fn_asm)
	popToA(fn_asm)
	printf(fn_asm, "%s", {"D=A-D\n@IF_TRUE"})
	printf(fn_asm, "%d" , {counter})
	printf(fn_asm, "%s", {"\nD; JLT\n@SP\nA=M\nM=0\n@end"})
	printf(fn_asm, "%d" , {counter+1})
	printf(fn_asm, "%s", {"\n0; JMP\n(IF_TRUE"})
	printf(fn_asm, "%d" , {counter})
	printf(fn_asm, "%s", {")\n@SP\nA=M\nM=-1\n(end"})
	printf(fn_asm, "%d" , {counter+1})
	printf(fn_asm, "%s", {")\n@SP\nM=M+1\n"})
	counter = counter + 2
end procedure

procedure andFunc(sequence splitLine, integer fn_asm)
	popToD(fn_asm)
	printf(fn_asm, "%s", {"@SP\nA=M-1\nM=D&M\n"})
end procedure

procedure orFunc(sequence splitLine, integer fn_asm)
	popToD(fn_asm)
	printf(fn_asm, "%s", {"@SP\nA=M-1\nM=D|M\n"})
end procedure

procedure notFunc(sequence splitLine, integer fn_asm)
	printf(fn_asm, "%s", {"@SP\nA=M-1\nM=!M\n"})
end procedure

------ walk_dir function------
-- will go throug all the files in the folder user enterd

function look_at(sequence path_name, sequence item) -- this is going to work on every file	

	ok = is_match("*.vm", item[D_NAME])	
	
	if ok then
		sequence fullpath = path_name&"\\"&item[D_NAME]		
		integer fn_vm = open(fullpath, "r")	-- item has a few elements in it, we just need the first 1 that has the file name
		filenameTmp = split(item[D_NAME],'.')
		integer fn_asm = open( path_name & "\\" & filenameTmp[1] & ".asm", "w") 		-- creates hello.asm file if doesn't exists
		object line
		
		if fn_vm = -1 then
			printf(1, "Can't open file %s\n", {fullpath})
			abort(1)
		end if 
		if (fn_asm) = -1  then
				printf(1, "Can't open file %s\n", {path_name&"\\hello.asm"})
				abort(1)
		end if 

	while sequence(line) entry do			-- exits while when line = -1
		sequence splitLine = split_any(line, " \n")	-- pg 448
		
		switch splitLine[1] do
			case "pop" then
				popFunc(splitLine, fn_asm)
			case "push" then
				pushFunc(splitLine, fn_asm)
			case "add" then
				addFunc(splitLine, fn_asm)
			case "sub" then
				subFunc(splitLine, fn_asm)
			case "neg" then
				negFunc(splitLine, fn_asm)
			case "eq" then
				eqFunc(splitLine, fn_asm)
			case "gt" then
				gtFunc(splitLine, fn_asm)
			case "lt" then
				ltFunc(splitLine, fn_asm)
			case "and" then
				andFunc(splitLine, fn_asm)	
			case "or" then
				orFunc(splitLine, fn_asm)
			case "not" then
				notFunc(splitLine, fn_asm)
			
			case else
		end switch
		
		entry
		line = gets(fn_vm)

	end while		

		close(fn_vm)
		close(fn_asm)

	end if
	return 0		
	
end function



exit_code = walk_dir("C:\\Users\\User\\Desktop\\ekronot\\nand2tetris\\projects\\07", routine_id("look_at"), 1)
if exit_code = -1 then
	printf(OUT, "Folder doesn't exists\n")
else
	printf(OUT, "Executed\n")
end if

any_key ("\n   Press any key to close this Window... ")