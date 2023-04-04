* @ValidationCode : MjotNzc1NTI5NTQ6Q3AxMjUyOjE2ODA2MDY2MTg1NDY6SVRTU0JORzotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 04 Apr 2023 16:40:18
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSSBNG
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
$PACKAGE APAP.REDOFCFI
SUBROUTINE REDO.FC.ENQ.BUILD.INS.TYPE.DE(DATA.OUT)
*
* ====================================================================================
*
*
* ====================================================================================
*
* Subroutine Type :Routine to  Nofile Enquiry
* Attached to     :REDO.FC.INS.TYPE.DE ENQUIRY
* Attached as     :Build routine attach to INSMNT.ISS.ENT.DE field in RCA.PRODUCT.CATALOG-PRODUCTS enquiry
* Primary Purpose :Put de Description of the instrument of REDO.INSTRUMENT.TYPE in O.DATA to enquiry
*
*
* Incoming:
* ---------
*
*
*
* Outgoing:
* ---------
* ---------
*
*
*-----------------------------------------------------------------------------------
* Modification History:
*
* Development for : Asociacion Popular de Ahorros y Prestamos
* Development by  : Bryan Torres (btorresalbornoz@temenos.com) - TAM Latin America
* Date            : Agosto 25 2011
* Modify History:
* Date           Who               Details
* 20 Dec 2012    Jorge Valarezo    Fixing Select
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*04-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*04-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
*=======================================================================

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.REDO.CREATE.ARRANGEMENT
    $INSERT I_F.REDO.ISSUE.ENTITY
    $INSERT I_F.REDO.INSTRUMENT.TYPE

*
*************************************************************************


    GOSUB INITIALISE
    GOSUB OPEN.FILES
    GOSUB CHECK.PRELIM.CONDITIONS
    IF PROCESS.GOAHEAD THEN
        GOSUB PROCESS
    END

*
RETURN
*
* ======
PROCESS:
* ======


    LOCATE "INSMNT.ISS.ENT.DE" IN D.FIELDS<1> SETTING PRO.POS THEN
        Y.ENTY=D.RANGE.AND.VALUE<PRO.POS>
    END

* CALL CACHE.READ(FN.REDO.ISSUE.ENTITY, Y.ENTY, R.REDO.ISSUE.ENTITY, Y.ERR)

* IF R.REDO.ISSUE.ENTITY THEN
*   Y.ENTY.DESCRIPTION=  R.REDO.ISSUE.ENTITY<REDO.IE.DESCRIPTION>

* SELECT.STATEMENT = "SELECT F.REDO.ISSUE.ENTITY WITH DESCRIPTION EQ '":Y.ENTY.DESCRIPTION:"'"
    SELECT.STATEMENT = "SELECT F.REDO.INSTRUMENT.TYPE WITH @ID LIKE '":Y.ENTY:"...'"
    Y.REDO.LOAN.LIST = ''
    LIST.NAME = ''
    SELECTED = ''
    SYSTEM.RETURN.CODE = ''
    Y.TYPE.PRODUCT = ''
    CALL EB.READLIST(SELECT.STATEMENT,Y.REDO.LOAN.LIST,LIST.NAME,SELECTED,SYSTEM.RETURN.CODE)
    LOOP
        REMOVE Y.ENTY.ID FROM Y.REDO.LOAN.LIST SETTING POS
    WHILE Y.ENTY.ID:POS

        CALL F.READ(FN.REDO.INSTRUMENT.TYPE, Y.ENTY.ID, R.REDO.INSTRUMENT.TYPE,F.REDO.INSTRUMENT.TYPE, Y.ERR)
        IF R.REDO.INSTRUMENT.TYPE THEN
            Y.ENTY.DESCRIPTION=  R.REDO.INSTRUMENT.TYPE<REDO.IT.INSTRUMENT.TYPE>

            DATA.OUT<-1>=Y.ENTY.ID:"*":Y.ENTY.DESCRIPTION
        END




    REPEAT
*  END

RETURN
*
* =========
OPEN.FILES:
* =========
*
    CALL OPF(FN.REDO.ISSUE.ENTITY,F.REDO.ISSUE.ENTITY)
    CALL OPF(FN.REDO.INSTRUMENT.TYPE,F.REDO.INSTRUMENT.TYPE)
RETURN
*
* =========
INITIALISE:
* =========
*
    LOOP.CNT = 1
    MAX.LOOPS = 1
    PROCESS.GOAHEAD = 1

    FN.REDO.ISSUE.ENTITY="F.REDO.ISSUE.ENTITY"
    F.REDO.ISSUE.ENTITY=""
    FN.REDO.INSTRUMENT.TYPE="F.REDO.INSTRUMENT.TYPE"
    F.REDO.INSTRUMENT.TYPE=""


RETURN

* ======================
CHECK.PRELIM.CONDITIONS:
* ======================
*

*
RETURN
*


END
