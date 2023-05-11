* @ValidationCode : MjoxOTE0NDg2MzMyOkNwMTI1MjoxNjgxMTA2MzkwMTkwOklUU1M6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 10 Apr 2023 11:29:50
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
SUBROUTINE REDO.GENERA.PWD
**
* Subroutine Type : VERSION
* Attached to     : REDO.CONFIRM.PASSWORD,NUEVO
* Attached as     : INPUT.RTN
* Primary Purpose : Change a password inputted by user
*-----------------------------------------------------------------------------
* MODIFICATIONS HISTORY
*
* 16/08/10 - First Version
*            Project: NCD Asociacion Popular de Ahorros y Prestamos (APAP)
*            Edgar Resendes - TAM Latin America
*            eresendes@temenos.com
* 20/10/11 - Update to TAM coding standards and fix for PACS00146412
*            Roberto Mondragon - TAM Latin America
*            rmondragon@temenos.com
** 10-04-2023 R22 Auto Conversion 
** 10-04-2023 Skanda R22 Manual Conversion - No changes
*-----------------------------------------------------------------------------

    $INSERT JBC.h ;* R22 Auto conversion
    $INSERT I_COMMON
    $INSERT I_EQUATE

    $INSERT I_F.REDO.CONFIRM.PASSWORD

    GOSUB INITIALIZE

RETURN

***********
INITIALIZE:
***********

    ID.USER = R.NEW(LOC.CP.ID.IBUSER)
    SRC.PWD = R.NEW(LOC.CP.NEW.PASSWORD.DEF)

    GOSUB CHANGE.PWD
    GOSUB ENC.PWD

RETURN

********
ENC.PWD:
********

    CIPHER = JBASE_CRYPT_BLOWFISH_BASE64
    STR.KEY = "SECRET KEY"
    ENC = ENCRYPT(SRC.PWD,STR.KEY,CIPHER)

    R.NEW(LOC.CP.NEW.PASSWORD.DEF) = ENC
    R.NEW(LOC.CP.CONFIRM.PASSWORD.DEF) = ENC

RETURN

***********
CHANGE.PWD:
***********

    OFS.REC = 'BROWSER.XML,,,,,,'
    OFS.REC := '<<?xml version="1.0" encoding="UTF-8"?>'
    OFS.REC := '<ofsSessionRequest xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" schemaLocation="\WEB-INF\xml\schema\ofsSessionRequest.xsd">'
    OFS.REC := '<requestType>UTILITY.ROUTINE</requestType>'
    OFS.REC := '<requestArguments>'
    OFS.REC := '<routineName>OS.PASSWORD</routineName>'
    OFS.REC := '<routineArgs>PROCESS.REPEAT:' : ID.USER
    OFS.REC := ':' : SRC.PWD : ':' : SRC.PWD : '</routineArgs>'
    OFS.REC := '</requestArguments>'
    OFS.REC := '</ofsSessionRequest>'
    OFS.MSG.ID = ''
    OFS.SOURCE.ID = 'ARCIB'
    OPTIONS = ''

    CALL OFS.POST.MESSAGE(OFS.REC, OFS.MSG.ID, OFS.SOURCE.ID, OPTIONS)

RETURN

END
