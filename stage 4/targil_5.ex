include std/pretty.e
include std/filesys.e -- needed for walk_dir
include std/wildcard.e -- needed for is_match
include std/console.e	-- needed for prompt_string	
include std/sequence.e  -- for split
include std/text.e -- for switch
include std/convert.e
include std/map.e
include std/math.e 
with trace

constant IN = 0, OUT = 1, FALSE = 0, TRUE = 1, EOF = -1
integer ok
sequence answer, filenameTmp
object exit_code
atom counter = 0
sequence keyword = {"class","constructor","function","method","field","static","var","int","char","boolean","void","true","false","null","this","let","do","if","else","while","return"}
sequence symbol = "{}()[].,;+-*/&|<>=~}"
sequence symbolAscii = {'{','}','(',')','[',']','.',',',';','+','-','*','/','&','|','<','>','=','~',' ','\t','"','\n'}
sequence keywordConstant={"true","false","null","this"}
------ walk dir vars
sequence fullpath, xmlpath, Txmlpath, fName
integer fn_jack, fn_vm, fn_xml, fn_Txml,fn_data
object line
sequence splitLine, db
integer current

-----------------------------------------------------------
----------------------- targil 5 --------------------------
-----------------------------------------------------------
map class_scope_table = map:new() 
map methods_scope_table = map:new()
constant Table_NAME = 1, Table_TYPE = 2, Table_KIND = 3, Table_NUM = 4
sequence className, funcName
integer argCounter = 0, varCounter = 0, fieldCounter = 0, staticCounter = 0, ifCounter = 0, subroutineDecFlag = 0

-----------------------------------------------------------
----------------------- push / pop ------------------------
-----------------------------------------------------------
procedure printPop(sequence vKind, object vNum)
	printf(fn_vm, "pop " & vKind & " ")
	print(fn_vm, vNum)
	printf(fn_vm, "\n")
end procedure

procedure printPush(sequence vKind, integer vNum)
	printf(fn_vm, "push " & vKind & " ")
	printf(fn_vm,"%d", {vNum})
	printf(fn_vm, "\n")
end procedure

---------------------- classTag --------------------------
procedure classTag(sequence res)
	className = res[2][2]
	clear(class_scope_table)
	fieldCounter = 0
	staticCounter = 0
	ifCounter = 0
end procedure

---------------------- classVarDecTag --------------------------
procedure classVarDecTag(sequence res)
	if equal(res[1][2], "static") then
		for i=3 to length(res) by 2 do			
			put(class_scope_table, res[i][2], {res[2][2] ,"static" , staticCounter}) -- name, type, static, counter
			staticCounter = staticCounter + 1
		end for

	else
		for i=3 to length(res) by 2 do			
			put(class_scope_table, res[i][2], {res[2][2] ,"this" , fieldCounter}) -- name, type, field, counter
			fieldCounter = fieldCounter + 1
		end for
	end if
end procedure

---------------------- SubroutineDecTag --------------------------
procedure SubroutineDecTag(sequence res)
	clear(methods_scope_table)
	argCounter = 0
	varCounter = 0
	subroutineDecFlag = 0
	funcName=res[3][2]
	if equal(res[1][2], "method") then
		put(methods_scope_table, "this", {className ,"argument" , argCounter}) -- name, type, argument, counter
		argCounter = argCounter + 1
	end if
	
	sequence funcType = res[1][2]
	switch funcType do
		case "constructor" then
			subroutineDecFlag = 1
		case "function" then
			subroutineDecFlag = 2
		case "method" then
			subroutineDecFlag = 3			
	end switch
end procedure

---------------------- PrintSubroutineDec --------------------------
procedure PrintSubroutineDec()
	if not(subroutineDecFlag = 0) then
		printf(fn_vm, "function " & className & "." & funcName & " %d\n", varCounter)
	    printf(fn_vm, "ffff")

	end if
	switch subroutineDecFlag do
		case 0 then
		case 1 then
			printPush("constant", fieldCounter)
			printf(fn_vm,"call Memory.alloc 1\n")
			printPop("pointer", to_integer(0))
			subroutineDecFlag = 0
		case 2 then
			subroutineDecFlag = 0
		case 3 then
			printPush("argument", 0)
			printPop("pointer", to_integer(0))
			subroutineDecFlag = 0
	end switch
end procedure

---------------------- parameterListTag --------------------------
procedure parameterListTag(sequence res)
	if sequence(res) and equal(res ,{}) then
		-- do nothing
	else
		for i=1 to length(res) by 3 do			
		put(methods_scope_table, res[i+1][2], {res[i][2] ,"argument" , argCounter}) -- name, type, argument, counter
		argCounter = argCounter + 1
	end for
	end if
end procedure

---------------------- varDecTag --------------------------
procedure varDecTag(sequence res)
	for i=3 to length(res) by 2 do			
		put(methods_scope_table, res[i][2], {res[2][2] ,"local" , varCounter}) -- name, type, var, counter
		varCounter = varCounter + 1
	end for
end procedure

---------------------- pushVarName --------------------------
procedure pushVarName(sequence res)
	object data = get(methods_scope_table, res)	-- this var appears in class_scope_table	
	if atom(data) and data = 0 then	
		data = get(class_scope_table, res)  -- this var appears in methods_scope_table
	end if
	printPush(data[2],data[3])
end procedure

---------------------- popVarName --------------------------
procedure popVarName(sequence res)
	object data = get(methods_scope_table, res)	-- this var appears in class_scope_table	
	if atom(data) and data = 0 then	
		data = get(class_scope_table, res)  -- this var appears in methods_scope_table
	end if
	printPop(data[2],data[3])
end procedure

---------------------- pushVar[Expression] --------------------------
procedure pushVarPlusExpression(sequence var, sequence exp)
	expressionTag(exp)
	pushVarName(var)
	printf(fn_vm , "add\n")
	printf(fn_vm , "pop pointer 1\n")	
	printf(fn_vm , "push that 0\n")	
end procedure

---------------------- letStatementTag --------------------------
procedure letStatementTag(sequence res)
	if equal(res[3][2],"=") then -- let var = something;
		expressionTag(res[4][2])
		popVarName(res[2][2])
	else							-- let var[exp] = something;
		expressionTag(res[4][2])
		pushVarName(res[2][2])
		printf(fn_vm , "add\n")
		expressionTag(res[7][2])
		printPop("temp", 0)
		printPop("pointer", 1)
		printPush("temp", 0)
		printPop("that", 0)
	end if
end procedure

---------------------- ifStatementTag --------------------------
procedure ifStatementTag(sequence res)
	integer localCounter = ifCounter
	ifCounter = ifCounter + 1
	
	expressionTag(res[3][2])		-- condition
	
	printf(fn_vm, "if-goto IF_TRUE")	-- if the condition is TRUE, jump to inside
	printf(fn_vm, "%d" , {localCounter})
	printf(fn_vm, "\n")
	printf(fn_vm, "goto IF_FALSE")		-- if the condition isn't TRUE, jump to FALSE
	printf(fn_vm, "%d" , {localCounter})
	printf(fn_vm, "\n")
	printf(fn_vm, "label IF_TRUE")		-- if TRUE, jump to here
	printf(fn_vm, "%d" , {localCounter})
	printf(fn_vm, "\n")
	
	ParsingFunc(res[6][2])				-- if TRUE do
	
	printf(fn_vm, "goto end")		-- if the condition is TRUE, jump to over "else"
	printf(fn_vm, "%d" , {localCounter})
	printf(fn_vm, "\n")
	
	printf(fn_vm, "label IF_FALSE")	-- if FALSE
	printf(fn_vm, "%d" , {localCounter})
	printf(fn_vm, "\n")	
	
	if length(res) > 7 then
		ParsingFunc(res[10])		-- else
	end if
	
	printf(fn_vm, "label end")		-- end if
	printf(fn_vm, "%d" , {localCounter})
	printf(fn_vm, "\n")
	
end procedure

---------------------- whileStatementTag --------------------------
procedure whileStatementTag(sequence res)
	integer localCounter = ifCounter
	ifCounter = ifCounter + 1
	
	printf(fn_vm, "label WHILE_CONDITION") -- beginning of while
	printf(fn_vm, "%d" , {localCounter})
	printf(fn_vm, "\n")
	
	expressionTag(res[3][2])    		-- condition
	
	printf(fn_vm, "not\n")
	printf(fn_vm, "if-goto IF_FALSE")  -- condition is FALSE, goto end while
	printf(fn_vm, "%d" , {localCounter})
	printf(fn_vm, "\n")

	ParsingFunc(res[6][2])    -- if TRUE do statements

	printf(fn_vm, "goto WHILE_CONDITION")  -- goto beginning
	printf(fn_vm, "%d" , {localCounter})
	printf(fn_vm, "\n")
  
	printf(fn_vm, "label IF_FALSE")  -- end while, if condition is FALSE
	printf(fn_vm, "%d" , {localCounter})
	printf(fn_vm, "\n")
end procedure

---------------------- doStatementTag --------------------------
procedure doStatementTag(sequence res)
	sequence subCall = {}
	for i=2 to length(res) do
		subCall = append(subCall, res[i])
	end for
	
	subroutineCall(subCall)
	printPop("temp", 0)
end procedure

---------------------- returnStatementTag --------------------------
procedure returnStatementTag(sequence res)
	sequence temp = res[2][2]
	if sequence(res[2][2]) and equal(res[2][2],";") then
		printPush("constant", 0)
	else
		expressionTag(res[2][2])
	end if
	printf(fn_vm , "return\n")
end procedure

---------------------- stringConstantTag --------------------------
procedure stringConstantTag(sequence res)
	integer len = length(res)
	printPush("constant", len)
	printf(fn_vm , "call String.new 1\n")
	
	for i=1 to len do
		printPush("constant", to_integer(res[i]))
		printf(fn_vm , "call String.appendChar 2\n")
	end for
end procedure

---------------------- termTag --------------------------
procedure termTag(sequence res)
	sequence tag = res[1][1]
	switch tag do
		case "integerConstant" then
			printPush("constant", to_integer((res[1][2])))
		case "stringConstant" then
			stringConstantTag(res[1][2])
		case "keyword" then
			switch res[1][2] do
				case "null" then
					printPush("constant", 0)
				case "true" then
					printPush("constant", 0)
					printf(fn_vm , "not \n")
				case "false" then
					printPush("constant", 0)
				case "this" then
					printPush("pointer", 0)
			end switch
		case "identifier" then
			if (length(res) =  1) then	-- varName
				pushVarName(res[1][2])
			else
				object val = res[2][2]
				switch val do
					case "[" then	-- varName [expression]
						pushVarPlusExpression(res[1][2], res[3][2])
					case "." then	-- className|varName.subroutineName(expressionList)
						subroutineCall(res)
					case "(" then	-- subroutineName(expressionList)
						subroutineCall(res)
					case else	-- varName
						pushVarName(res[1][2])
				end switch
			end if
		case "symbol" then
			sequence val = res[1][2]
			switch val do
				case "(" then
					expressionTag(res[2][2])	-- TODO try?
				case "-" then
					termTag(res[2][2])
					unaryOpTag(res[1][2])
				case "~" then
					termTag(res[2][2])
					unaryOpTag(res[1][2])
			end switch
	end switch
end procedure
---------------------- subroutineCall --------------------------					
procedure subroutineCall(sequence res)
	sequence var = res[2][2]
	switch var do 
		case "." then	-- className|varName.subroutineName(expressionList)
			integer numArg = ceil(length(res[5][2])/2)
			
			object data = get(methods_scope_table, res[1][2])
			if atom(data) and data = 0 then	
				data = get(class_scope_table, res[1][2])
			end if
			
			if sequence(data) then			-- if its a varName
				printPush(data[2],data[3])
				numArg = numArg + 1
				expressionListTag(res[5][2])	--push parameter list
				printf(fn_vm , "call " & data[1] & res[2][2] & res[3][2] & " ")	-- print call aaa.bbb
				printf(fn_vm , "%d" , {numArg})
				printf(fn_vm , "\n")
			else			
				expressionListTag(res[5][2])	--push parameter list
				printf(fn_vm , "call " & res[1][2] & res[2][2] & res[3][2] & " ")	-- print call aaa.bbb
				printf(fn_vm , "%d" , {numArg})
				printf(fn_vm , "\n")
			end if
		case "(" then	-- subroutineName(expressionList)
			integer numArg = ceil(length(res[3][2])/2)
			printPush("pointer", 0)
			numArg = numArg + 1
			expressionListTag(res[3][2])	--push parameter list
			printf(fn_vm , "call " & className & "." & res[1][2] & " ")	-- print call aaa.bbb
			printf(fn_vm , "%d" , {numArg})
			printf(fn_vm , "\n")
	end switch
end procedure

---------------------- opTag --------------------------
procedure opTag(object res)
	sequence var = res
	switch var do
		case "+" then
			printf(fn_vm , "add\n")
		case "-" then
			printf(fn_vm , "sub\n")
		case "*" then
			printf(fn_vm , "call Math.multiply 2\n")
		case "/" then 
			printf(fn_vm , "call Math.divide 2\n")
		case "&amp;" then
			printf(fn_vm, "and\n")
		case "|" then
			printf(fn_vm, "or\n")
		case "&lt;" then
			printf(fn_vm, "lt\n")
		case "&gt;" then
			printf(fn_vm, "gt\n")
		case "=" then
			printf(fn_vm, "eq\n")
		case else
			printf(OUT, "misstake in opTag()")
	end switch
end procedure

---------------------- unaryOpTag --------------------------
procedure unaryOpTag(object res)
	switch res[1] do
		case '-' then
			printf(fn_vm , "neg \n")
		case '~' then
			printf(fn_vm , "not \n")
		case else
			printf(OUT, "misstake in unaryOpTag()")
	end switch
end procedure

---------------------- expressionTag --------------------------
procedure expressionTag(sequence res)
	termTag(res[1][2])
	
	for i=2 to length(res) by 2 do	
		sequence val12 = res[i][2]
		sequence val13 = res[i+1][2]
		termTag(res[i+1][2])
		opTag(res[i][2])
	end for
end procedure

---------------------- expressionListTag --------------------------
procedure expressionListTag(sequence res)
	if sequence(res) and equal(res ,{}) then
		-- do nothing	
	else
		expressionTag(res[1][2])
		for i=2 to length(res) by 2 do			
			expressionTag(res[i+1][2])
		end for
	end if
end procedure  

---------------------- ParsingFunc --------------------------
procedure ParsingFunc(sequence res)  
	if sequence(res) and equal(res ,{}) then	

		-- do nothing	
	--elsif isString(res) then							-- for string value
	
	elsif isString(res[1]) then						-- for tag and sequence values
		switch res[1] do
			case "class" then
				classTag(res[2])
				ParsingFunc(res[2])
			case "classVarDec" then
				classVarDecTag(res[2])
			case "subroutineDec" then				
				SubroutineDecTag(res[2])
				ParsingFunc(res[2])
			case "parameterList" then
				parameterListTag(res[2])
			case "varDec" then
				varDecTag(res[2])
			case "letStatement" then
				PrintSubroutineDec()
				letStatementTag(res[2])
			case "ifStatement" then
				PrintSubroutineDec()
				ifStatementTag(res[2])
			case "whileStatement" then
				PrintSubroutineDec()
				whileStatementTag(res[2])
			case "doStatement" then
				PrintSubroutineDec()
				doStatementTag(res[2])
			case "returnStatement" then
				PrintSubroutineDec()
				returnStatementTag(res[2])
			case else
				if sequence(res[2]) and not(isString(res[2])) then	
					for i=1 to length(res[2]) do			
						ParsingFunc(res[2][i])
					end for
				end if
		end switch
	else												-- for sequence value
		for i=1 to length(res) do			
			ParsingFunc(res[i])
		end for				
	end if	
end procedure

-----------------------------------------------------------
-------------------- print Tokenizing - xml ---------------
-----------------------------------------------------------
procedure TokenizingXML(sequence res, integer level=0)  
	if sequence(res) and equal(res ,{}) then		-- do nothing
			
	elsif isString(res) then							-- for string value
		printf(fn_xml , res)
	else
		if isString(res[1]) then						-- for tag and sequence values
			printf(fn_xml , "<" & res[1] &">")
					
			if not(isString(res[2])) then 
				printf(fn_xml , "\n")
				--printTabFunc(level)
			end if
					
				for i=2 to length(res) do			
					TokenizingXML(res[i], level+1)
				end for

			printf(fn_xml , "<" & "/" & res[1] & ">\n")
			--printTabFunc(level)
		else												-- for sequence value
				for i=1 to length(res) do			
					TokenizingXML(res[i], level+1)
				end for				
		end if
	end if	
end procedure

function isString(sequence s)  
	integer str = 0
	if sequence(s) and equal(s ,{}) then
		str = 0
	elsif sequence(s) and atom(s[1]) then
		str = 1
	end if
	return str
end function

-----------------------------------------------------------
-------------------- print Tokenizing - xml ---------------
-----------------------------------------------------------
procedure oldTokenizingPrintFunc(sequence res, integer level=0)  
	printf(fn_xml , "<" & res[1] &">")
	
	if (sequence(res[2])and equal(res[2],{})) then
		printf(fn_xml , "\n")
		printTabFunc(level-1)
		printf(fn_xml , "<" & "/" & res[1] & ">")
	elsif (atom(res[2][1])) then
		printf(fn_xml ,( " " & res[2] & " "))
		printf(fn_xml , "<" & "/" & res[1] & ">")
	else
		for i=1 to length(res[2]) do
			printf(fn_xml , "\n")
			printTabFunc(level)
			oldTokenizingPrintFunc(res[2][i], level+1 )
		end for
		printf(fn_xml , "\n")
		printTabFunc(level-1)
		printf(fn_xml , "<" & "/" & res[1] & ">")
	end if	
end procedure

procedure printTabFunc(integer level)
	for i=0 to level do
		printf(fn_xml, 32 & 32)
	end for
end procedure

-----------------------------------------------------------
------------- print Parsing - T.xml -----------------------
-----------------------------------------------------------
procedure ParsingTXML(sequence res)  
	printf(fn_Txml , "<" & "tokens" &">\n")
		TxmlPrintFunc(res)
	printf(fn_Txml , "<" & "/" & "tokens" & ">")
end procedure

procedure TxmlPrintFunc(sequence res)  
	if (sequence(res[2])and equal(res[2],{})) then
	elsif (atom(res[2][1])) then
		printf(fn_Txml , "<" & res[1] &">")
		printf(fn_Txml ,( " " & res[2] & " "))
		printf(fn_Txml , "<" & "/" & res[1] & ">\n")
	else
		for i=1 to length(res[2]) do
			TxmlPrintFunc(res[2][i])
		end for
	end if
	
end procedure

-----------------------------------------------------------
------------------- internal func -------------------------
-----------------------------------------------------------

---------------- skipNote ------------------- checked!
-- if /* was readed, skip until */
procedure skipNote()  
  integer last = 0
  current = getc(fn_jack)
  
  while (not (last = '*' and current = '/')) do
	last = current
	current = getc(fn_jack)
  end while
  -- after finding */ read the next char
  nextChar()
end procedure

---------------- skipSpaceEnterTab -------------------
procedure skipSpaceEnterTab()
	while (current = ' ' or current = '\n' or current = 9) do
		current = getc(fn_jack)
	end while
end procedure

------------------ skip ------------------------- checked!
-- skip on: space , // ... , /* ... */ , \n
procedure skip() 
	while (current = '/' or current = ' ' or current = '\n' or current = 9) do
		skipSpaceEnterTab()
		if current = '/' then
			nextChar()
			switch current do
				-- for: //
				case '/' then
					gets(fn_jack)
					nextChar()
				-- for: /*
				case '*' then
					skipNote()
				case else
					printf(OUT, "error in skip")
			end switch
		end if
	end while
end procedure

---------------- nextChar -------------------
procedure nextChar()
	current = getc(fn_jack)
	skipSpaceEnterTab()
end procedure

---------------------- readWord --------------------- checked!
function readWord(sequence cond = symbolAscii)
  sequence word = ""  
  
  while (not find(current, cond)) do
	word = word & current
	current = getc(fn_jack)
  end while
  --printf(1,"%s\n",{word})
  skipSpaceEnterTab()
  return word
end function



-----------------------------------------------------------
---------------- Lexical elements ------------------------
-----------------------------------------------------------

function StringConstantFunc()
	nextChar()	-- for '"'
	sequence db = {"stringConstant", readWord({'"'})}
	nextChar()	-- for '"'
	return db	
end function

function integerConstantFunc()
	sequence word = ""  
  
  while (find(current, {48,49,50,51,52,53,54,55,56,57})) do
	word = word & current
	current = getc(fn_jack)
  end while
  
  skipSpaceEnterTab()
	return {"integerConstant", word}
end function

function identifierFunc(sequence curWord="")	
	if (equal(curWord, "")) then
		curWord = readWord()
	end if
	return {"identifier", curWord}
end function

-----------------------------------------------------------
---------------- Program structure ------------------------
-----------------------------------------------------------

---------------- classFunc -------------------
function classFunc()
	sequence db = {}  
	skip()
	db = append(db,{"keyword", "class"})
	readWord()
	db = append(db, classNameFunc())
	skip()
	db = append(db, {"symbol", "{"})
	nextChar()
	skip()
	db = db & classVarAndSubroutineDec()
	db = append(db, {"symbol", "}"})
	nextChar()
	return {"class", db}
end function
-------------- classVarAndSubroutineDec ------------
function classVarAndSubroutineDec()
	sequence db = {}
	sequence curWord = readWord()

	while (equal(curWord, "field") or equal(curWord, "static")) do
		db = append(db, classVarDecFunc(curWord))
		skip()
		curWord = readWord()
	end while
	
	while (current != '}') do
		db = append(db, subroutineDecFunc(curWord))
		skip()
		curWord = readWord()
	end while
	
	return db
end function
---------------- classVarDec -------------------
function classVarDecFunc(sequence curWord)
	sequence db = {}
	
	db = append(db,{"keyword", curWord})
	db = append(db, typeFunc())
	db = append(db, varNameFunc())
		
	while (current != ';') do
		db = append(db, {"symbol", ","})
		nextChar()
		db = append(db, varNameFunc())
	end while
	
	db = append(db,{"symbol", ";"})
	nextChar()
	
	return {"classVarDec", db}
end function
--------------------- type -----------------------
function typeFunc(sequence curWord="")
	
	if (equal(curWord, "")) then
		curWord = readWord()
	end if
	
	if (equal(curWord, "int") or equal(curWord, "char") or equal(curWord, "boolean")) then
		db = {"keyword", curWord}
	else
		db = {"identifier", curWord}
	end if
	
	return db
end function
---------------- subroutineDec -------------------	
function subroutineDecFunc(sequence curWord)
	sequence db = {}

	db = append(db,{"keyword", curWord})
	
	curWord = readWord()	
	if (equal(curWord, "void")) then
		db = append(db,{"keyword", "void"})
	else
		db = append(db,typeFunc(curWord))
	end if
	
	db = append(db,subroutineNameFunc())
	db = append(db, {"symbol", "("})
	nextChar()
	
	db = append(db,parameterListFunc())
	
	db = append(db, {"symbol", ")"})
	nextChar()
	db = append(db,subroutineBodyFunc())
	
	return {"subroutineDec", db}
end function
------------ parameterList -------------
function parameterListFunc()
	sequence db = {}
	
	if (current != ')') then
		db = append(db,typeFunc())
		db = append(db,varNameFunc())

		while (current = ',') do
			db = append(db, {"symbol", ","})
			nextChar()
			db = append(db,typeFunc())
			db = append(db,varNameFunc())
		end while
	end if
	
	return {"parameterList", db}
end function
------------ subroutineBody -------------
function subroutineBodyFunc()
	sequence db = {}
	skip()
	db = append(db, {"symbol", "{"})
	nextChar()
	skip()
	
	while (current = 'v') do
		db = append(db, varDecFunc())
	end while
	
	db = append(db, statementsFunc())
	skip()
	db = append(db, {"symbol", "}"})
	nextChar()
	skip()
	return {"subroutineBody", db}
end function
------------ varDec -------------
function varDecFunc()
	sequence db = {}
	
	db = append(db, {"keyword", readWord()})
	db = append(db, typeFunc())
	db = append(db, varNameFunc())
	
	while (current != ';') do
		db = append(db, {"symbol", ","})
		nextChar()
		db = append(db, varNameFunc())
	end while
	
	db = append(db,{"symbol", ";"})	
	nextChar()
	
	return {"varDec", db}
end function

------------ className -------------
function classNameFunc()
	return identifierFunc()
end function
--------- subroutineName -----------
function subroutineNameFunc()
	return identifierFunc()
end function
----------- varName ---------------
function varNameFunc()
	return identifierFunc()
end function


-----------------------------------------------------------
------------------- Statements ---------------------------
-----------------------------------------------------------

---------------------statements------------------------
function statementsFunc()
	sequence db = { }
	skip()
	-- there is only one option to statement - in subroutineBody, before }
	while (not (current = '}')) do
		db = append(db, statementFunc())
		skip()
	end while

	return {"statements", db}
end function

-----------------------statement-------------------------
function statementFunc()
	sequence db = {}
	sequence word = readWord()
	switch word do
		case "let" then
			db = db & letStatementFunc()
		case "if" then
			db = db & ifStatementFunc()
		case "while" then
			db = db & whileStatementFunc()
		case "do" then
			db = db & doStatementFunc()
		case "return" then
			db = db & returnStatementFunc()				
		case else
		printf(fn_vm, "%s", {db&"2"})
			--printf(OUT, "error in statementFunc")
	end switch
	
return db
end function

------------------------letStatementFunc------------------------
function letStatementFunc()
	sequence db = { }
	db = append(db,{"keyword", "let"})
	-- next char was read by readWord()
		

	db = append(db, varNameFunc())
	
	if not (current = '=') then
		db = append(db, {"symbol","["})
		nextChar()
		
		db = append(db, expressionFunc())
		
		db = append(db, {"symbol","]"})
		nextChar()
	end if
	
	db = append(db,{"symbol", "="})
	nextChar()
	
	db = append(db, expressionFunc())
	
	db = append(db,{"symbol", ";"})
	nextChar()

return {"letStatement", db}
end function

-----------------------ifStatementFunc-------------------------
function ifStatementFunc()
	sequence db = { }
	db = append(db,{"keyword", "if"})
	-- readWord in statementsFunc made skipSpaceEnterTab

	db = append(db,{"symbol", "("})
	nextChar()

	db = append(db, expressionFunc())

	db = append(db,{"symbol", ")"})
	nextChar()
	skip() -- for: if (true) /* ... */  or if (true) //

	db = append(db,{"symbol", "{"})
	nextChar()
	--statementsFunc() doing skip

	db = append(db, statementsFunc())

	db = append(db,{"symbol", "}"})
	nextChar()
	skip() -- for: if(){} /* ... */  if(){} //
	if (current = 'e') then
		if equal(readWord(),"else") then
			db = append(db, {"keyword","else"})
			db = append(db, {"symbol","{"})
			nextChar()
			
			db = append(db, statementsFunc())
			
			db = append(db, {"symbol","}"})
			nextChar()
			skip() -- for: else{} /* ... */  else{} //
		end if
	end if
	
return {"ifStatement", db}
end function

-------------------------whileStatementFunc-----------------------
function whileStatementFunc()
	sequence db = {}
	db = append(db,{"keyword", "while"})
	-- readWord in statementsFunc made skipSpaceEnterTab

	db = append(db,{"symbol", "("})
	nextChar()

	db = append(db, expressionFunc())

	db = append(db,{"symbol", ")"})
	nextChar()
	skip() -- for: while (true) /* ... */  or while (true) //

	db = append(db,{"symbol", "{"})
	nextChar()
	--statementsFunc() doing skip

	db = append(db, statementsFunc())

	db = append(db,{"symbol", "}"})
	nextChar()
	skip() -- for: if(){} /* ... */  if(){} //
	return {"whileStatement", db}
end function

-----------------------doStatementFunc-------------------------
function doStatementFunc()
	sequence db = {}
	db = append(db,{"keyword", "do"})
	-- next char was read by readWord()
	
	db = db & subroutineCallFunc()	
	
	db = append(db,{"symbol", ";"})
	nextChar()		

return {"doStatement" ,  db}
end function

---------------------------returnStatementFunc---------------------
function returnStatementFunc()
	sequence db = {}
	db = append(db,{"keyword", "return"})
	-- next char was read by readWord()
	
	if not (current = ';') then
		db = append(db, expressionFunc())
	end if
	
	db = append(db,{"symbol", ";"})
	nextChar()		

return {"returnStatement" , db}
end function

-----------------------------------------------------------
------------------- Expressions ---------------------------
-----------------------------------------------------------

---------------------- expressionFunc --------------------------
function expressionFunc()
sequence db = {}
db = append(db,termFunc())

while not (current = ';' or current = ')' or current = ']' or current = ',') do
		db = append(db, opFunc())
		db = append(db, termFunc())
end while

return {"expression", db}
end function

---------------------- termFunc --------------------------
function termFunc()
	sequence db = {}, word

	if(current = '-' or current = '~') then
		db = append(db, unaryOpFunc())
		db = append(db, termFunc())
	elsif(current >= 48 and current <= 57)then
		db = append(db, integerConstantFunc())
	elsif(current = '"')then
		db = append(db, StringConstantFunc())
	elsif(current = '(')then
		db = append(db, {"symbol", "("})
		nextChar()
		db = append(db, expressionFunc())
		db = append(db,{"symbol", ")"})
		nextChar()
	else
		word = readWord()
		if(find (word,keywordConstant)) then
			db = append(db, keywordConstantFunc(word))
		elsif(current = '(') or (current = '.') then
			db = db & subroutineCallFunc(word)
		elsif(current = '[')then
			db = append(db,identifierFunc(word))
			db = append(db,{"symbol", "["})
			nextChar()
			db = append(db, expressionFunc())
			db = append(db,{"symbol", "]"})
			nextChar()
		else
			db = append(db,identifierFunc(word))
		end if
	end if
	
return {"term", db}
end function

---------------------- subroutineCallFunc --------------------------
function subroutineCallFunc(sequence word = "")
	if equal(word,"") then
		word = readWord()
	end if
	
	sequence db = {identifierFunc(word)}
	if(current = '.') then
		db = append(db,{"symbol", "."})
		nextChar()
		db = append(db, identifierFunc())
	end if

	db = append(db,{"symbol", "("})
		nextChar()
		db = append(db, expressionListFunc())
		db = append(db,{"symbol", ")"})
		nextChar()
	return db
end function

---------------------- expressionListFunc --------------------------
function expressionListFunc()
	sequence db = {}
	if( not (current = ')')) then
		db = append(db, expressionFunc())
		while not (current = ')') do
			db = append(db, {"symbol", ","})
			nextChar()
			db = append(db, expressionFunc())
		end while
	end if
return {"expressionList", db}
end function

---------------------- opFunc --------------------------
-- only space can be before the operator.
-- read the next char when finished
function opFunc()
sequence db = {}
	switch current do
		case '+' then
			db = db & "+"
		case '-' then
			db = db & "-"
		case '*' then
			db = db & "*"
		case '/' then 
			db = db & "/"
		case '&' then
			db = db & "&amp;"	
		case '|' then
			db = db & "|"
		case '<' then
			db = db & "&lt;"
		case '>' then
			db = db & "&gt;"
		case '=' then
			db = db & "="				
		case else
			printf(OUT, "misstake in opFunc()")
	end switch

-- read the next char
nextChar()
return {"symbol" ,db}
end function

---------------------- unaryOpFunc --------------------------
-- only space can be before the operator.
-- read the next char when finished
function unaryOpFunc()
sequence db = {}
skipSpaceEnterTab()
	switch current do
		case '-' then
			db = db & "-"
		case '~' then
			db = db & "~"
		case else
			printf(OUT, "misstake in unaryOpFunc()")
	end switch

-- read the next char
nextChar()
return {"symbol" , db}
end function

---------------------- keywordConstantFunc --------------------------
function keywordConstantFunc(sequence word = "")
	if equal(word,"") then
		word = readWord()
	end if
return {"keyword", word}
end function


-----------------------------------------------------------
-------------------------- MAIN ---------------------------
-----------------------------------------------------------


-------------------------------------------------
------ walk_dir function------
-- will go throug all the files in the folder user enterd


function look_at(sequence path_name, sequence item) --  this is going to work on every file  

  ok = is_match("*.jack", item[D_NAME])  
  
  if ok then    -- if this file is ok
    fullpath = path_name&"\\"&item[D_NAME] -- build the path to the file. item has a few elements in it, we just need the first 1 that has the file name
    fn_jack = open(fullpath, "r")  
    filenameTmp = split(item[D_NAME],'.')
    fn_vm = open( path_name & "\\" & filenameTmp[1] & ".vm", "w")     
    --fn_xml = open( path_name & "\\" & filenameTmp[1] & "Our" & ".xml", "w")
	--fn_data = open( path_name & "\\" & filenameTmp[1] & ".txt", "w")
    --fn_Txml = open( path_name & "\\" & filenameTmp[1] & "TOur" & ".xml", "w")
    
    if fn_jack = -1 then
      printf(1, "Can't open file %s\n", {fullpath})
      abort(1)
    end if 
    if (fn_vm) = -1  then
        printf(1, "Can't open file %s\n", {path_name&"\\hello.asm"})
        abort(1)
    end if 
--    if fn_Txml = -1 then
--      printf(1, "Can't open file %s\n", {fullpath})
--      abort(1)
--    end if  
	
	nextChar()
   sequence db = classFunc()
	ParsingFunc(db)
    close(fn_jack)
    close(fn_vm)
 

  end if
  return 0    
  
end function


--------Code stars here:--------

exit_code = walk_dir("C:\\Users\\User\\Desktop\\ekronot\\nand2tetris\\projects\\11", routine_id("look_at"), 1)
if exit_code = -1 then
	printf(OUT, "Folder doesn't exists\n")
else
	printf(OUT, "Executed\n")
end if

while 1<100 do end while
