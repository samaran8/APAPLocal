* @ValidationCode : MjozNTU4NzA4NDE6Q3AxMjUyOjE2ODI0MTIzNjIxNzk6SGFyaXNodmlrcmFtQzotMTotMTowOjE6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 25 Apr 2023 14:16:02
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOVER
SUBROUTINE REDO.V.VAL.ISSUE.REQ.DESC.CLAIM
*-----------------------------------------------------------------------------
*----------------------------------------------------------------------------------------------------
*DESCRIPTION : A validation routine is written to update the DESCRIPTION.CLIAM field from
*the local table REDO.SLA.PARAM
*
*-----------------------------------------------------------------------------------------------------
*-----------------------------------------------------------------------------------------------------
* * Input / Output
* --------------
* IN     : -NA-
* OUT    : -NA-
*-----------------------------------------------------------------------------------------------------
* COMPANY NAME : APAP
* DEVELOPED BY : SUDHARSANAN S
* PROGRAM NAME : REDO.V.VAL.ISSUE.REQ.DESC.CLAIM
*-----------------------------------------------------------------------------------------------------
* Modification History :
*-----------------------
* DATE             WHO                REFERENCE         DESCRIPTION
* 30.07.2010      SUDHARSANAN S     ODR-2009-12-0283  INITIAL CREATION
* ----------------------------------------------------------------------------------------------------
*Modification History
*DATE                       WHO                         REFERENCE                                   DESCRIPTION
*17-04-2023            Conversion Tool             R22 Auto Code conversion                      FM TO @FM VM TO @VM
*17-04-2023              Samaran T                R22 Manual Code conversion                         No Changes
*-----------------------------------------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_F.REDO.ISSUE.REQUESTS
    $INSERT I_F.REDO.SLA.PARAM

    GOSUB INIT
    GOSUB PROCESS
RETURN
*------------------------------------------------------------------------------------------------------
INIT:
*------------------------------------------------------------------------------------------------------
    FN.REDO.SLA.PARAM = 'F.REDO.SLA.PARAM'
    F.REDO.SLA.PARAM = ''
    CALL OPF(FN.REDO.SLA.PARAM,F.REDO.SLA.PARAM)

RETURN
*--------------------------------------------------------------------------------------------------------
PROCESS:
*--------------------------------------------------------------------------------------------------------
    IF AF EQ ISS.REQ.PRODUCT.TYPE THEN
        IF (VAL.TEXT) AND COMI NE 'OTROS' AND R.NEW(ISS.REQ.ACCOUNT.ID) EQ '' THEN
            ETEXT="EB-ACCT.NO.MAND"
            CALL STORE.END.ERROR
        END
        Y.PRDT.TYPE = COMI
*        VAR.PRDT.TYPE = FIELD(Y.PRDT.TYPE,"-",2)
        Y.TYPE = R.NEW(ISS.REQ.TYPE)
*        VAR.TYPE = FIELD(Y.TYPE,"-",2)
        Y.ID = Y.TYPE:'-':Y.PRDT.TYPE
        IF Y.TYPE NE '' THEN
            GOSUB DES.CLAIM
        END
    END
    IF AF EQ ISS.REQ.TYPE THEN
        Y.PRDT.TYPE = R.NEW(ISS.REQ.PRODUCT.TYPE)
*        VAR.PRDT.TYPE = FIELD(Y.PRDT.TYPE,"-",2)
        Y.TYPE = COMI
*        VAR.TYPE = FIELD(Y.TYPE,"-",2)
        IF Y.TYPE EQ 'SOLICITUD' THEN
            Y.ID = Y.TYPE:'-':Y.PRDT.TYPE
            IF Y.PRDT.TYPE NE '' THEN
                GOSUB DES.CLAIM
            END
        END ELSE
            AF = ISS.REQ.TYPE
            ETEXT ='EB-NOT.VALID.TYPE'
            CALL STORE.END.ERROR
        END
    END
RETURN
*-------------------------------------------------------------------------------------------
DES.CLAIM:
*-------------------------------------------------------------------------------------------

    CALL F.READ(FN.REDO.SLA.PARAM,Y.ID,R.SLA.PARAM,F.REDO.SLA.PARAM,PARA.ERR)
    IF R.SLA.PARAM THEN
        R.NEW(ISS.REQ.SLA.ID) = Y.ID
    END ELSE
        AF = ISS.REQ.PRODUCT.TYPE
        ETEXT ='EB-NOT.VALID.PRODUCT':@FM:Y.PRDT.TYPE:@VM:Y.TYPE
        CALL STORE.END.ERROR
    END
RETURN
*----------------------------------------------------------------------------------------------
END
