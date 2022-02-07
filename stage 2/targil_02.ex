-- mordechai rothkoff 302888300
-- elad de-roos 305689408

include std/filesys.e -- needed for walk_dir
include std/wildcard.e -- needed for is_match
include std/console.e	-- needed for prompt_string	
include std/sequence.e  -- for split
include std/text.e -- for switch
include std/convert.e
include std/io.e
with trace

-- address to use:
-- C:\Euphoria\bin\programs

constant IN = 0, OUT = 1, FALSE = 0, TRUE = 1, EOF = -1
integer ok
sequence answer, filenameTmp, funcName = ""
object exit_code
atom counter = 0
integer fn_asm
sequence splitLine
--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

procedure popToD()
		printf(fn_asm, "%s", {"@SP\nM=M-1\nA=M\nD=M\n"})
end procedure

procedure popToA()
		printf(fn_asm, "%s", {"@SP\nM=M-1\nA=M\nA=M\n"})
end procedure
--~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- D=M+D
procedure popFunc1(object offset, object segment)	
	printf(fn_asm, "%s", {"@" & offset & "\nD=A\n@" & segment & "\nD=M+D\n@13\nM=D\n"})
	popToD()
	printf(fn_asm, "%s", {"@13\nA=M\nM=D\n"})
end procedure

-- D=A+D
procedure popFunc2(object offset, object segment)	
	printf(fn_asm, "%s", {"@" & offset & "\nD=A\n@" & segment & "\nD=A+D\n@13\nM=D\n"})
	popToD()
	printf(fn_asm, "%s", {"@13\nA=M\nM=D\n"})
end procedure
--~~~~~~~~~~~~~~~~
procedure popFunc()

object segment

	switch splitLine[2] do
	
		case "local" then
			segment = "LCL"
			popFunc1(splitLine[3], segment)
			
		case "argument" then
			segment = "ARG"
			popFunc1(splitLine[3], segment)
			
		case "this" then
			segment = "THIS "
			popFunc1(splitLine[3], segment)
			
		case "that" then
			segment = "THAT "
			popFunc1(splitLine[3], segment)
			
		case "temp" then
			segment = "5"
			popFunc2(splitLine[3], segment)		
			
		case "pointer" then
			segment = "THIS"
			popFunc2(splitLine[3], segment)
			
		case "static" then
			segment = filenameTmp[1] & "." & splitLine[3]			
			popToD()
			printf(fn_asm, "%s", {"@" & segment & "\nM=D\n"})
			
		case else
			printf(fn_asm, "%s", {"ERROR"})
	end switch
			
end procedure

--~~~~~~~~~~~~~~~~~~~~~ push ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A=M+D
procedure pushFunc1(object offset, object segment)	
	printf(fn_asm, "%s", {"@" & offset & "\nD=A\n@" & segment & "\nA=M+D\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"})
end procedure

-- A=A+D
procedure pushFunc2(object offset, object segment)	
	printf(fn_asm, "%s", {"@" & offset & "\nD=A\n@" & segment & "\nA=A+D\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"})
end procedure
--~~~~~~~~~~~~~~~~~~~
procedure pushFunc()

object segment

	switch splitLine[2] do
	
		case "local" then
			segment = "LCL"
			pushFunc1(splitLine[3], segment)
			
		case "argument" then
			segment = "ARG"
			pushFunc1(splitLine[3], segment)
			
		case "this" then
			segment = "THIS"
			pushFunc1(splitLine[3], segment)
			
		case "that" then
			segment = "THAT"
			pushFunc1(splitLine[3], segment)
			
		case "temp" then
			segment = "5"
			pushFunc2(splitLine[3], segment)		
			
		case "pointer" then
			segment = "THIS"
			pushFunc2(splitLine[3], segment)
			
		case "static" then
			segment = filenameTmp[1] & "." & splitLine[3]			
			printf(fn_asm, "%s", {"@" & segment & "\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"})
			
		case "constant" then
			printf(fn_asm, "%s", {"@"& splitLine[3] &"\nD=A\n@SP\nA=M\nM=D\n@SP\nM=M+1\n"})
			
		case else
			printf(fn_asm, "%s", {"ERROR"})
	end switch
	
end procedure

--~~~~~~~~~~~~~~~~~~~~~ logic commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

procedure addFunc()
	popToD()
	printf(fn_asm, "%s", {"@SP\nA=M-1\nM=M+D\n"})
end procedure

procedure subFunc()
	popToD()
	printf(fn_asm, "%s", {"@SP\nA=M-1\nM=M-D\n"})
end procedure

procedure negFunc()
	popToD()
	printf(fn_asm, "%s", {"@SP\nA=M\nM=-D\n@SP\nM=M+1\n"})
end procedure

--~~~~~~~~~~~~~~~~~~~ jamp functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

procedure eqFunc()
--	trace(1)
	popToD()
	popToA()
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

procedure gtFunc()
	popToD()
	popToA()
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

procedure ltFunc()
	popToD()
	popToA()
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

--~~~~~~~~~~~~~~~~ arithmetic commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

procedure andFunc()
	popToD()
	printf(fn_asm, "%s", {"@SP\nA=M-1\nM=D&M\n"})
end procedure

procedure orFunc()
	popToD()
	printf(fn_asm, "%s", {"@SP\nA=M-1\nM=D|M\n"})
end procedure

procedure notFunc()
	printf(fn_asm, "%s", {"@SP\nA=M-1\nM=!M\n"})
end procedure

--~~~~~~~~~~~~~~~~~~~~~ label ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

procedure labelFunc()
	printf(fn_asm, "%s", {"(" & filenameTmp[1] & "." & splitLine[2] & ")\n"})
end procedure

procedure gotoFunc()
	printf(fn_asm, "%s", {"@" & filenameTmp[1] & "." & splitLine[2] & "\n0; JMP\n"})
end procedure

procedure ifgotoFunc()
	popToD()
	printf(fn_asm, "%s", {"@" & filenameTmp[1] & "." & splitLine[2] & "\nD; JNE\n"})
end procedure

--~~~~~~~~~~~~~~~~~~~~ push D ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

procedure pushD()
	printf(fn_asm, "%s", {"@SP\nA=M\nM=D\n@SP\nM=M+1\n"})
end procedure

--~~~~~~~~~~~~~~~~~~  call f n ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

procedure callFunc()
	-- @return_address
	printf(fn_asm, "%s", {"@" & filenameTmp[1] & "."})
	printf(fn_asm, "%d" , {counter})
	printf(fn_asm, "%s", {"\nD=A\n"})
	pushD()
	
	-- push LCL
	printf(fn_asm, "%s", {"@LCL\nD=M\n"})
	pushD()
	
	-- push ARG
	printf(fn_asm, "%s", {"@ARG\nD=M\n"})
	pushD()
	
	-- push THIS
	printf(fn_asm, "%s", {"@THIS\nD=M\n"})
	pushD()
	
	-- push THAT
	printf(fn_asm, "%s", {"@THAT\nD=M\n"})
	pushD()
	
	-- ARG = SP-n-5
	printf(fn_asm, "%s", {"@SP\nD=M\n@"})
	printf(fn_asm, "%s", {splitLine[3] & "\n"})
	printf(fn_asm, "%s", {"D=D-A\n@5\nD=D-A\n@ARG\nM=D\n"})

	--	LCL = SP
	printf(fn_asm, "%s", {"@SP\nD=M\n@LCL\nM=D\n"})
	
	--	goto f
	printf(fn_asm, "%s", {"@" & splitLine[2] & "\n"})
	printf(fn_asm, "%s", {"0; JMP\n("})
	printf(fn_asm, "%s", {filenameTmp[1] & "."})
	printf(fn_asm, "%d", {counter})
	printf(fn_asm, "%s", {")\n"})

	counter = counter + 1

end procedure

 --~~~~~~~~~~~~~~ function declaration ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

procedure fkFunc()
	-- (f)
	printf(fn_asm, "%s", {"(" & splitLine[2] & ")\n"})
	
	integer k = to_integer(splitLine[3])
	
	-- repeat k times: push 0
	for i = 1 to k do
		printf(fn_asm, "%s", {"@0\nD=A\n"})
		pushD()
	end for
		
end procedure
 
 --~~~~~~~~~~~~~~~~~~ return ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
 procedure returnFunc()
	-- FRAME = LCL
	printf(fn_asm, "%s", {"@LCL\nD=M\n@13\nM=D\n"})
	
	-- RET = *(FRAME-5)		-- the return address
	printf(fn_asm, "%s", {"@5\nD=D-A\nA=D\nD=M\n@14\nM=D\n"})
	
	-- *ARG = POP()		-- the return value
	popToD()
	printf(fn_asm, "%s", {"@ARG\nA=M\nM=D\n"})
	
	-- SP = ARG+1
	printf(fn_asm, "%s", {"D=A\n@SP\nM=D+1\n"})
	
	-- THAT = *(FRAME-1)
	printf(fn_asm, "%s", {"@13\nA=M-1\nD=M\n@THAT\nM=D\n"})
	
	-- THIS = *(FRAME-2)
	printf(fn_asm, "%s", {"@13\nA=M-1\nA=A-1\nD=M\n@THIS\nM=D\n"})
	
	-- ARG = *(FRAME-3)
	printf(fn_asm, "%s", {"@13\nA=M-1\nA=A-1\nA=A-1\nD=M\n@ARG\nM=D\n"})
	
	-- LCL = *(FRAME-4)
	printf(fn_asm, "%s", {"@13\nA=M-1\nA=A-1\nA=A-1\nA=A-1\nD=M\n@LCL\nM=D\n"})
	
	-- goto RET
	printf(fn_asm, "%s", {"@14\nA=M\n0;JMP\n"})	
	
end procedure
 
 --~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 


------ walk_dir function------
-- will go throug all the files in the folder user enterd

function translateVmToAsm(sequence path_name, sequence item) -- this is going to work on every file	

	ok = is_match("*.vm", item[D_NAME])	
	
	if ok then
		sequence fullpath = path_name&"\\"&item[D_NAME]		
		integer fn_vm = open(fullpath, "r")	-- item has a few elements in it, we just need the first 1 that has the file name
		filenameTmp = split(item[D_NAME],'.')
		fn_asm = open( path_name & "\\" & filenameTmp[1] & ".asm", "w") 		-- creates hello.asm file if doesn't exists
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
		splitLine = split_any(line, " \n")	-- pg 448
		
		switch splitLine[1] do
			case "pop" then
				popFunc()
			case "push" then
				pushFunc()
			case "add" then
				addFunc()
			case "sub" then
				subFunc()
			case "neg" then
				negFunc()
			case "eq" then
				eqFunc()
			case "gt" then
				gtFunc()
			case "lt" then
				ltFunc()
			case "and" then
				andFunc()	
			case "or" then
				orFunc()
			case "not" then
				notFunc()
			case "label" then
				labelFunc()
			case "goto" then
				gotoFunc()
			case "if-goto" then
				ifgotoFunc()
			case "call" then
				callFunc()
			case "function" then
--				funcName = splitLine[2]
				fkFunc()
			case "return" then
--				funcName = ""
				returnFunc()
				
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


function mergeAsmFiles(sequence path_name, sequence item)
	ok = is_match("*.asm", item[D_NAME])	
	
	if ok then
		integer fileToMerge = open(path_name&"\\"&item[D_NAME], "r")
		
		sequence fileData = read_file(fileToMerge, io:TEXT_MODE)
		
		printf(fn_asm, "\n\n\n")
		write_file(fn_asm, fileData, io:TEXT_MODE)
		
		close(fileToMerge)
	end if
	return 0		
end function


function SysExist(sequence path_name, sequence item)
	ok = is_match("Sys.vm", item[D_NAME])	
		
	if ok then
		-- creates new asm file with the name of the folder
				
		sequence split_path_name = split(path_name, "\\")
		integer pathLength = length(split_path_name)
		sequence folderName = split_path_name[pathLength]		
		
		fn_asm = open( path_name & "\\" & folderName & ".asm", "w") 
		close(fn_asm)
		
		fn_asm = open( path_name & "\\" & folderName & ".asm", "a") 
		
		-- ~~~ Initialize ~~~~~~
		printf(fn_asm, "%s", {"@256\nD=A\n@SP\nM=D\n"})
		splitLine = {"call", "Sys.init", "0"}
		callFunc()
		
		-- ~~~~ merge the files ~~~~~~~~~~~
		walk_dir(path_name, routine_id("mergeAsmFiles"))		
		
		close(fn_asm)
		
	end if
	return 0
end function


-------- Main: --------

--answer = prompt_string ("enter full path name to use:\n")
exit_code = walk_dir("C:\\Users\\User\\Desktop\\ekronot\\nand2tetris\\projects\\08", routine_id("translateVmToAsm"), TRUE)
--trace(1)
exit_code = walk_dir("C:\\Users\\User\\Desktop\\ekronot\\nand2tetris\\projects\\08", routine_id("SysExist"), TRUE)
if exit_code = -1 then
	printf(OUT, "Folder doesn't exists\n")
else
	printf(OUT, "Executed\n")
end if

any_key ("\n   Press any key to close this Window... ")


