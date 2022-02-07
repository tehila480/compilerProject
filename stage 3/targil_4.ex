include std/filesys.e -- needed for walk_dir
include std/wildcard.e -- needed for is_match
include std/console.e	-- needed for prompt_string	
include std/sequence.e  -- for split
include std/text.e -- for switch
with trace
constant IN = 0, OUT = 1, FALSE = 0, TRUE = 1, EOF = -1
sequence keyword = {"class","constructor","function","method","field","static","var","int","char","boolean","void","true","false","null","this","let","do","if","else","while","return"}
sequence symbol = "{}()[].,;+-*/&|<>=~}"
sequence symbolAscii = {'{','}','(',')','[',']','.',',',';','+','-','*','/','&','|','<','>','=','~','"'}
integer ok,ok2,ok3,ok4,ok5,ok6,ok7,ok8,ok9,ok10,i,j
sequence fullpath , filenameTmp
integer fn_jack,fn_Txml,fn_xml
integer tav
object exit_code,line,line2
sequence splitLine 
sequence tok={}

function tokens()
     printf(fn_Txml,"%s",{"<tokens>\n"})
	 tav=getc(fn_jack)
	--tok=sequence(tav)
	 --if tav='c' then printf(fn_Txml,"%s",{tav}) end if
    while 1 do 
	if (tav>='a' and tav<='z') or (tav>='A' and tav<='Z') or tav='_' then
        tok=append(tok,tav)
	    tav=getc(fn_jack)	
		while (tav>='a' and tav<='z')or (tav>='A' and tav<='Z') or (tav='_' )or (tav>='0' and tav<='9') do
             tok=append(tok,tav)
	         tav=getc(fn_jack)  
		end while
	    i=find(tok,keyword)
		    if i then
			     printf(fn_Txml,"%s",{"<keyword> "&tok&" </keyword>\n"})
			else
			    printf(fn_Txml,"%s",{"<identifier> "&tok&" </identifier>\n"})
		    end if
	    tok={}
	elsif tav>='0' and tav<='9' then
	    tok=append(tok,tav)
	    tav=getc(fn_jack)
		while tav>='0' and tav<='9' do
		     tok=append(tok,tav)
	         tav=getc(fn_jack)
	    end while
	printf(fn_Txml,"%s",{"<integerConstant> "&tok&" </integerConstant>\n"})
	tok={}
	elsif tav='/' then  
	    tav=getc(fn_jack)
		if tav='/' then
		    tav=getc(fn_jack)
		     while tav!='\n' do
			     tav=getc(fn_jack)
			end while
	     	tav=getc(fn_jack)
		elsif tav='*' then 
		    tav=getc(fn_jack)
		     while 1 do
			    if tav='*'then
			         tav=getc(fn_jack)
				     if tav='/' then
				         tav=getc(fn_jack)
				         exit
				    end if

				else 
				     tav=getc(fn_jack)	
				end if
			end while 
		else 
             printf(fn_Txml,"%s",{"<symbol> "&'/'&" </symbol>\n"})		
		end if
	elsif (tav=' ') or (tav='\n') or (tav='\t') then tav=getc(fn_jack)  
	elsif tav='"' then 
	     tok={}
	     tav=getc(fn_jack)
	     while tav!='"' do 
		 tok=append(tok,tav)
		  tav=getc(fn_jack)
		end while
		printf(fn_Txml,"%s",{"<stringConstant> "&tok&" </stringConstant>\n"})
		tav=getc(fn_jack)
		tok={}
		 
	else 
	     j=find(tav,symbolAscii)
		 if j then
	         if tav='<' then printf(fn_Txml,"%s",{"<symbol> "&"&lt;"&" </symbol>\n"}) 
		     elsif tav='>' then printf(fn_Txml,"%s",{"<symbol> "&"&gt;"&" </symbol>\n"})
             elsif tav='"' then printf(fn_Txml,"%s",{"<symbol> "&"&quet;"&" </symbol>\n"})
            elsif tav='&' then printf(fn_Txml,"%s",{"<symbol> "&"&amp;"&" </symbol>\n"})		
		    else printf(fn_Txml,"%s",{"<symbol> "&tav&" </symbol>\n"}) end if
		     tav=getc(fn_jack) 
	    else exit end if	
    end if		
	end while
	   printf(fn_Txml,"%s",{"</tokens>\n"})	 
	 return 0
end function

function classFunc()
     printf(fn_xml,"<class>\n")
	 line=gets(fn_Txml)
     line=gets(fn_Txml)
     printf(fn_xml,"%s",{line})
	 line=gets(fn_Txml)
     classNameFunc()
     line=gets(fn_Txml)
     printf(fn_xml,"%s",{line})
     line=gets(fn_Txml)
	 splitLine = split_any(line, " \n")
	 ok2=is_match(splitLine[2],"static")
	 ok3=is_match(splitLine[2],"field")
	 while ok2 or ok3 do
	     classVarDecFunc()
		 line=gets(fn_Txml)
	    splitLine = split_any(line, " \n")
		ok2=is_match(splitLine[2],"static")
	    ok3=is_match(splitLine[2],"field")
	 end while
	 ok2=is_match(splitLine[2],"constructor")
	 ok3=is_match(splitLine[2],"function")
	 ok4=is_match(splitLine[2],"method")
	 while ok2 or ok3 or ok4 do
	      subroutineDecFunc()
		  line=gets(fn_Txml)
	      splitLine = split_any(line, " \n")
		  ok2=is_match(splitLine[2],"constructor")
	    ok3=is_match(splitLine[2],"function")
	     ok4=is_match(splitLine[2],"method") 
    end while
	printf(fn_xml,"%s",{line})
	line=gets(fn_Txml)
     printf(fn_xml,"</class>\n")
     return 0
end function

function classVarDecFunc()
     printf(fn_xml,"<classVarDec>\n")
	 printf(fn_xml,"%s",{line})
	 line=gets(fn_Txml)
	 typeFunc()
	 line=gets(fn_Txml)
	line2=line
	 varNameFunc()
	 line=gets(fn_Txml)
	  splitLine = split_any(line, " \n")
	ok2=is_match(splitLine[2],",")
	while ok2 do
        printf(fn_xml,"%s",{line}) 
         line=gets(fn_Txml)
		 line2=line
         varNameFunc()
		 line=gets(fn_Txml)
	     splitLine = split_any(line, " \n")
	     ok2=is_match(splitLine[2],",")
	end while
	 printf(fn_xml,"%s",{line})
	 printf(fn_xml,"</classVarDec>\n")
    return 0
end function
function typeFunc()
     splitLine = split_any(line, " \n")
	ok2=is_match(splitLine[2],"int")
	 ok3=is_match(splitLine[2],"char")
	 ok4=is_match(splitLine[2],"boolean")
	 if ok2 or ok3 or ok4 then
	      printf(fn_xml,"%s",{line})
    else
	classNameFunc()
	end if 
return 0
end function
function subroutineDecFunc()
     printf(fn_xml,"<subroutineDec>\n")
	 printf(fn_xml,"%s",{line})
	 line=gets(fn_Txml)
	 splitLine = split_any(line, " \n")
	ok2=is_match(splitLine[2],"void")
	if ok2 then printf(fn_xml,"%s",{line})
	else typeFunc() end if 
     line=gets(fn_Txml)
	 subroutineNameFunc()
     line=gets(fn_Txml)
	 printf(fn_xml,"%s",{line})
	 line2=line
	 line=gets(fn_Txml)
	 splitLine = split_any(line, " \n")
	 ok2=is_match(splitLine[2],")")
	 if ok2 then
	 printf(fn_xml,"<parameterList>\n")
	 printf(fn_xml,"</parameterList>\n")
	 printf(fn_xml,"%s",{line})
	 else
	 prameterListFunc() 
	 --line=gets(fn_Txml)
	 printf(fn_xml,"%s",{line})
	 end if
	 line=gets(fn_Txml)
	 subroutineBodyFunc()
	 printf(fn_xml,"</subroutineDec>\n") 
return 0
end function
function prameterListFunc()
 printf(fn_xml,"<parameterList>\n")
  --  splitLine = split_any(line, " \n")
	--ok2=is_match(splitLine[2],")")
	--if not ok2 then 	
	     typeFunc()
		 line=gets(fn_Txml)
		 line2=line
		 varNameFunc()
		 line=gets(fn_Txml)
		 splitLine = split_any(line, " \n")
	     ok2=is_match(splitLine[2],",")
		 while ok2 do
		  printf(fn_xml,"%s",{line})  
		  line=gets(fn_Txml)
		  typeFunc()
		  line=gets(fn_Txml)
		  line2=line
		 varNameFunc()
		 line2=line
		 line=gets(fn_Txml)
		 splitLine = split_any(line, " \n")
	     ok2=is_match(splitLine[2],",")
		 end while
		 --line=line2
	--elsif ok2 then
	 --   line=line2 
	--end if
	printf(fn_xml,"</parameterList>\n")
return 0
end function
function subroutineBodyFunc()
     printf(fn_xml,"<subroutineBody>\n")
     printf(fn_xml,"%s",{line}) 
	 line=gets(fn_Txml)
	 splitLine = split_any(line, " \n")
	 ok2=is_match(splitLine[2],"var")
	 while ok2 do 
	    varDecFunc()
	     line=gets(fn_Txml)
	     splitLine = split_any(line, " \n")
	     ok2=is_match(splitLine[2],"var")
	 end while
	 statementsFunc()
	 --line=gets(fn_Txml)
	 printf(fn_xml,"%s",{line})
	 printf(fn_xml,"</subroutineBody>\n")
    return 0
end function
function varDecFunc()
     printf(fn_xml,"<varDec>\n")
      printf(fn_xml,"%s",{line})
	  line=gets(fn_Txml)
	  typeFunc()
	  line=gets(fn_Txml)
	  line2=line
	  varNameFunc() 
	   line=gets(fn_Txml)
		 splitLine = split_any(line, " \n")
	     ok2=is_match(splitLine[2],",")
		 while ok2 do
		  printf(fn_xml,"%s",{line})  
		  line=gets(fn_Txml)
		  line2=line
		 varNameFunc()
		 line=gets(fn_Txml)
		 splitLine = split_any(line, " \n")
	     ok2=is_match(splitLine[2],",")
		 end while
		 printf(fn_xml,"%s",{line})
		 printf(fn_xml,"</varDec>\n")
     return 0
end function
function classNameFunc()
     printf(fn_xml,"%s",{line})
	 return 0
end function
function subroutineNameFunc()
     printf(fn_xml,"%s",{line})
	 return 0
end function
function varNameFunc()
     printf(fn_xml,"%s",{line2})
	 return 0
end function
function statementsFunc()
     printf(fn_xml,"<statements>\n")
	 splitLine = split_any(line, " \n")
	 ok2=is_match(splitLine[2],"let")
	 ok3=is_match(splitLine[2],"if")
	 ok4=is_match(splitLine[2],"while")
	 ok5=is_match(splitLine[2],"do")
	 ok6=is_match(splitLine[2],"rturn")
	 while ok2 or ok3 or ok4 or ok5 or ok6 do
	    integer i1= statementFunc()
		 if i1=0 then
		 --line2=line
		line=gets(fn_Txml) end if
		splitLine = split_any(line, " \n")
	    ok2=is_match(splitLine[2],"let")
	    ok3=is_match(splitLine[2],"if")
	     ok4=is_match(splitLine[2],"while")
	    ok5=is_match(splitLine[2],"do")
	    ok6=is_match(splitLine[2],"return")
	end while
	--line=line2
     printf(fn_xml,"</statements>\n")
     return 0
end function
function statementFunc()
     splitLine = split_any(line, " \n")
	 ok2=is_match(splitLine[2],"let")
	 ok3=is_match(splitLine[2],"if")
	 ok4=is_match(splitLine[2],"while")
	 ok5=is_match(splitLine[2],"do")
	 ok6=is_match(splitLine[2],"return")
	 if ok2 then letStatementFunc()
	 elsif ok3 then 
	 integer i1=ifStatementFunc()
	    if i1=1 then return 1 end if
	 elsif ok4 then whileStatementFunc()
	 elsif ok5 then doStatementFunc()
	 else  returnStatementFunc() end if
     return 0
end function
function letStatementFunc()
     printf(fn_xml,"<letStatement>\n")
    printf(fn_xml,"%s",{line})
	line=gets(fn_Txml)
	line2=line
	varNameFunc()
	line=gets(fn_Txml)
	splitLine = split_any(line, " \n")
	 ok2=is_match(splitLine[2],"[")
	 if ok2 then
	    printf(fn_xml,"%s",{line})
	    line=gets(fn_Txml)
		expressionFunc()
		--line=gets(fn_Txml)
		printf(fn_xml,"%s",{line})
		line=gets(fn_Txml)
	 end if
	 printf(fn_xml,"%s",{line})
	 line=gets(fn_Txml)
     expressionFunc()
	--line=gets(fn_Txml)
	printf(fn_xml,"%s",{line})
	printf(fn_xml,"</letStatement>\n")
     return 0
	 
end function
function ifStatementFunc()
     printf(fn_xml,"<ifStatement>\n")
    printf(fn_xml,"%s",{line})
    line=gets(fn_Txml)
    printf(fn_xml,"%s",{line})
     line=gets(fn_Txml)
	 expressionFunc()
    --line=gets(fn_Txml)
    printf(fn_xml,"%s",{line})
    line=gets(fn_Txml)
    printf(fn_xml,"%s",{line})	
    line=gets(fn_Txml)
	statementsFunc()
    --line=gets(fn_Txml)
    printf(fn_xml,"%s",{line})
	line2=line
	line=gets(fn_Txml)
     splitLine = split_any(line, " \n")
	 ok2=is_match(splitLine[2],"else")
    if ok2 then
	printf(fn_xml,"%s",{line})
    line=gets(fn_Txml)
    printf(fn_xml,"%s",{line})
	line=gets(fn_Txml)
	statementsFunc()
    --line=gets(fn_Txml)
    printf(fn_xml,"%s",{line})	
	 else printf(fn_xml,"</ifStatement>\n") return 1 end if 
	 printf(fn_xml,"</ifStatement>\n")
     return 0
	 
end function
function whileStatementFunc()
     printf(fn_xml,"<whileStatement>\n")
    printf(fn_xml,"%s",{line})
    line=gets(fn_Txml)
    printf(fn_xml,"%s",{line})  
    line=gets(fn_Txml)
	expressionFunc()
    --line=gets(fn_Txml)
    printf(fn_xml,"%s",{line})
    line=gets(fn_Txml)
    printf(fn_xml,"%s",{line})
    line=gets(fn_Txml)
     statementsFunc()	
    --line=gets(fn_Txml)
    printf(fn_xml,"%s",{line})	
     printf(fn_xml,"</whileStatement>\n")  	
     return 0
end function
function doStatementFunc()
     printf(fn_xml,"<doStatement>\n")
    printf(fn_xml,"%s",{line}) 
    line=gets(fn_Txml)
     line2=line
     line=gets(fn_Txml)	 
	subroutineCallFunc()
    line=gets(fn_Txml)
    printf(fn_xml,"%s",{line})
     printf(fn_xml,"</doStatement>\n")	
     return 0
end function
function returnStatementFunc()
     printf(fn_xml,"<returnStatement>\n")
    printf(fn_xml,"%s",{line}) 
    line=gets(fn_Txml)
    splitLine = split_any(line, " \n")
	 ok2=is_match(splitLine[2],";")
	 if ok2 then
	 printf(fn_xml,"%s",{line}) 
	 printf(fn_xml,"</returnStatement>\n")
	 return 0
	 end if 
	 expressionFunc()
    --line=gets(fn_Txml)
    printf(fn_xml,"%s",{line})
	printf(fn_xml,"</returnStatement>\n")
     return 0
end function
function expressionFunc()
	 printf(fn_xml,"<expression>\n")
    integer i1=termFunc()
if i1=0 then
   line=gets(fn_Txml) end if
    splitLine = split_any(line, " \n")
	 ok2=is_match(splitLine[2],"+")
	 ok3=is_match(splitLine[2],"-")
	 ok4=is_match(splitLine[2],"*")
	 ok5=is_match(splitLine[2],"/")
	 ok6=is_match(splitLine[2],"&amp;")
	 ok7=is_match(splitLine[2],"|")
	 ok8=is_match(splitLine[2],"&lt;")
	 ok9=is_match(splitLine[2],"&gt;")
	 ok10=is_match(splitLine[2],"=")
	 while ok2 or ok3 or ok4 or ok5 or ok6 or ok7 or ok8 or ok9 or ok10 do
	    opFunc()
        line=gets(fn_Txml)
        i1=termFunc()
      if i1=0 then
		--line2=line
		line=gets(fn_Txml) end if
        splitLine = split_any(line, " \n")
	    ok2=is_match(splitLine[2],"+")
	     ok3=is_match(splitLine[2],"-")
	     ok4=is_match(splitLine[2],"*")
	      ok5=is_match(splitLine[2],"/")
	     ok6=is_match(splitLine[2],"&amp;")
	     ok7=is_match(splitLine[2],"|")
	     ok8=is_match(splitLine[2],"&lt;")
	     ok9=is_match(splitLine[2],"&gt;")
	     ok10=is_match(splitLine[2],"=")
    end while 
    --line=line2	
	 printf(fn_xml,"</expression>\n")
    return 0
end function
function termFunc()
 	 printf(fn_xml,"<term>\n")
     splitLine = split_any(line, " \n")
     ok2=is_match(splitLine[1],"<integerConstant>")	
	 ok3=is_match(splitLine[1],"<stringConstant>")
	 if ok2 or ok3 then
	 printf(fn_xml,"%s",{line})
     printf(fn_xml,"</term>\n")
     	 
	 return 0 end if
     ok2=is_match(splitLine[2],"true")	
	 ok3=is_match(splitLine[2],"false")
     ok4=is_match(splitLine[2],"null")	
	 ok5=is_match(splitLine[2],"this")
     if ok2 or ok3 or ok4 or ok5 then
     keywordConstantFunc()
     printf(fn_xml,"</term>\n")	 
	 return 0 end if
    ok2=is_match(splitLine[2],"(")
    if ok2 then
         printf(fn_xml,"%s",{line})	
		 line=gets(fn_Txml)
		 expressionFunc()
		 --line=gets(fn_Txml)
		 printf(fn_xml,"%s",{line})
	     printf(fn_xml,"</term>\n")	 
	     return 0 end if
    ok2=is_match(splitLine[2],"-")
     ok3=is_match(splitLine[2],"~")
    if ok2 or ok3 then 
	    unaryOpFunc()
		line=gets(fn_Txml)	
        integer i1=termFunc()
	     printf(fn_xml,"</term>\n")
        if i1=1 then return 1 else		 
	     return 0 end if end if
	line2=line	
     line=gets(fn_Txml)
     splitLine = split_any(line, " \n")
     ok2=is_match(splitLine[2],"(")	
	 ok3=is_match(splitLine[2],".")
     if ok2 or ok3 then
        subroutineCallFunc()
	     printf(fn_xml,"</term>\n")	 
	     return 0 end if
     ok2=is_match(splitLine[2],"[") 
     if ok2 then 
        --line=line2
		--line2=line
        varNameFunc()
       -- line=gets(fn_Txml)
         printf(fn_xml,"%s",{line})
		 line=gets(fn_Txml)
		 --line=gets(fn_Txml)
		 expressionFunc()
		 printf(fn_xml,"%s",{line})
		  printf(fn_xml,"</term>\n")	 
	     return 0 end if
		 --line=line2
		 varNameFunc()
		 -- printf(fn_xml,"%s",{line})

 	 printf(fn_xml,"</term>\n")	 
    return 1
end function
function subroutineCallFunc()
     printf(fn_xml,"%s",{line2})
	--line=gets(fn_Txml)
	 splitLine = split(line)
     ok2=is_match(splitLine[2],"(")
       ok3=is_match(splitLine[2],".")	 
	   if ok2 then 
	    printf(fn_xml,"%s",{line})
	     line2=line
	    line=gets(fn_Txml) splitLine = split_any(line, " \n")
	    ok2=is_match(splitLine[2],")")
		if ok2 then 
		printf(fn_xml,"<expressionList>\n")
		printf(fn_xml,"</expressionList>\n")
		else
		expressionListFunc() 
		--line=gets(fn_Txml)
		end if
		printf(fn_xml,"%s",{line})
	else 
	     --printf(fn_xml,"<symbol> . </symbol>\n")
	     printf(fn_xml,"%s",{line})
	     line=gets(fn_Txml)
	    subroutineNameFunc()
		 line=gets(fn_Txml)
		 printf(fn_xml,"%s",{line})
		 line2=line
		 line=gets(fn_Txml)
		 splitLine = split_any(line, " \n")
	    ok2=is_match(splitLine[2],")")
		if ok2 then 
		printf(fn_xml,"<expressionList>\n")
		printf(fn_xml,"</expressionList>\n")
		else
		 expressionListFunc()
		 -- line=gets(fn_Txml) 
		 end if
		 printf(fn_xml,"%s",{line})
	end if
	 return 0
end function
function expressionListFunc()
printf(fn_xml,"<expressionList>\n")

	    expressionFunc()
		--line=gets(fn_Txml)
		splitLine = split_any(line, " \n")
	    ok2=is_match(splitLine[2],",")
		while ok2 do 
		printf(fn_xml,"%s",{line}) 
		line=gets(fn_Txml)
		expressionFunc()
		line2=line
		--line=gets(fn_Txml)
		splitLine = split_any(line, " \n")
	    ok2=is_match(splitLine[2],",")
		end while
		--line=line2
	printf(fn_xml,"</expressionList>\n")
     return 0
end function
function opFunc()
    printf(fn_xml,"%s",{line})
	return 0
end function
function keywordConstantFunc()
    printf(fn_xml,"%s",{line})
	return 0
end function
function unaryOpFunc()
    printf(fn_xml,"%s",{line})
	return 0
end function
function func1(sequence path_name, sequence item)

  ok = is_match("*.jack", item[D_NAME])  
  
  if ok then    -- if this file is ok
    fullpath = path_name&"\\"&item[D_NAME] -- build the path to the file. item has a few elements in it, we just need the first 1 that has the file name
    fn_jack = open(fullpath, "r")  
    filenameTmp = split(item[D_NAME],'.') 
    fn_Txml = open( path_name & "\\" & filenameTmp[1] & "T.xml1", "w")	
    
    if fn_jack = -1 then
      printf(1, "Can't open file %s\n", {fullpath})
      abort(1)
    end if 
    
    if fn_Txml = -1 then
      printf(1, "Can't open file %s\n", {fullpath})
      abort(1)
    end if
	tokens()
	close(fn_jack)
    close(fn_Txml)

  end if
  return 0    
end function

function func2(sequence path_name, sequence item)

  ok = is_match("*T.xml1", item[D_NAME])  
  
  if ok then    -- if this file is ok
    fullpath = path_name&"\\"&item[D_NAME] -- build the path to the file. item has a few elements in it, we just need the first 1 that has the file name
    fn_Txml = open(fullpath, "r")  
    filenameTmp = split(item[D_NAME],"T.") 
    fn_xml = open( path_name & "\\" & filenameTmp[1] & ".xml1", "w")	
  if (fn_Txml) = -1  then
        printf(1, "Can't open file %s\n", {path_name&"\\hello.asm"})
        abort(1)
    end if 
	if (fn_xml) = -1  then
        printf(1, "Can't open file %s\n", {path_name&"\\hello.asm"})
        abort(1)
    end if 
	classFunc()
    close(fn_xml)
	close(fn_Txml)

  end if
  return 0    
  
end function









--"C:\\Users\\User\\Desktop\\ekronot\\Exercises\\Targil4\\project 10"



exit_code = walk_dir("C:\\Users\\User\\Desktop\\ekronot\\Exercises\\Targil4\\project 10", routine_id("func1"), 1)
if exit_code = -1 then
	printf(OUT, "Folder doesn't exists\n")
else
	printf(OUT, "Executed\n")
end if
exit_code = walk_dir("C:\\Users\\User\\Desktop\\ekronot\\Exercises\\Targil4\\project 10", routine_id("func2"), 1)
if exit_code = -1 then
	printf(OUT, "Folder doesn't exists\n")
else
	printf(OUT, "Executed\n")
end if
while 1<100 do end while