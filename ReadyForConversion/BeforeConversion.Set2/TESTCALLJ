*-----------------------------------------------------------------------------
* <Rating>89</Rating>
*-----------------------------------------------------------------------------
  PROGRAM TESTCALLJ
  packageName = ''
  className = 'Logic.Hash'
  methodName = 'SHA1'
  param = 'S'

  CRT 'calling CALLJ'
  CALLJ packageName : className, methodName, param SETTING returnValue ON ERROR GOTO errHandler
  CRT 'recieved from Java ' : returnValue
  RETURN

errHandler:
  CRT 'Error!!'
  ERR = SYSTEM(0)
  BEGIN CASE
  CASE ERR = 1
    CRT 'JVM releated error'
  CASE ERR = 2
    CRT 'Cannot find the JVM. Check your environment variables'
  CASE ERR = 3
    CRT 'Cannot find class ' : className : '. Check your classpath'
  CASE ERR = 4
    CRT 'Unicode conversion ERROR'
  CASE ERR = 5
    CRT 'Cannot find method ' : methodName
  CASE ERR = 6
    CRT 'JVM releated error'
  CASE ERR = 8
    CRT 'JVM releated error'
  CASE 1  ;* default case
    CRT 'JVM releated error'
  END CASE
END
