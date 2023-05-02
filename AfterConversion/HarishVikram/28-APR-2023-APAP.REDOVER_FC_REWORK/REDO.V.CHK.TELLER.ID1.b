* @ValidationCode : MjoxMTAwMjkzNTY0OkNwMTI1MjoxNjgyNDEyMzQ1MTQ3OkhhcmlzaHZpa3JhbUM6LTE6LTE6MDoxOmZhbHNlOk4vQTpERVZfMjAyMTA4LjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 25 Apr 2023 14:15:45
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
SUBROUTINE REDO.V.CHK.TELLER.ID1
*---------------------------------------------------------------------------------------
*DESCRIPTION: This routine will default the teller id for the from teller attach to the
*version of TELLER,REDO.TILL.TRNS
*---------------------------------------------------------------------------------------
*IN  :  -NA-
*OUT :  -NA-
*****************************************************
*COMPANY NAME : APAP
*DEVELOPED BY : DHAMU S
*PROGRAM NAME : REDO.V.CHK.TELLER.ID
*----------------------------------------------------------------------------------------------
*Modification History:
*------------------------
*DATE               WHO                      REFERENCE                    DESCRIPTION
*9-6-2011        S.DHAMU                 ODR-2009-10-0525                INITIAL CREATION
*11-04-2023       Conversion Tool        R22 Auto Code conversion          No Changes
*11-04-2023       Samaran T               R22 Manual Code Conversion       No Changes
 
*-----------------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.TELLER
    $INSERT I_F.TELLER.ID

    GOSUB INIT
    GOSUB PROCESS
RETURN
******
INIT:
******

    FN.TELLER = 'F.TELLER'
    F.TELLER  = ''
    CALL OPF(FN.TELLER,F.TELLER)

    FN.TELLER.ID = 'F.TELLER.ID'
    F.TELLER.ID  = ''
    CALL OPF(FN.TELLER.ID,F.TELLER.ID)
RETURN
********
PROCESS:
********
    Y.REC.STATUS=R.NEW(TT.TE.RECORD.STATUS)
    IF Y.REC.STATUS EQ 'INA2' OR Y.REC.STATUS EQ 'INAU' THEN
        RETURN
    END ELSE
        SEL.CMD ="SELECT ":FN.TELLER.ID:" WITH STATUS EQ OPEN AND USER EQ ":OPERATOR
        CALL EB.READLIST(SEL.CMD,SEL.LIST,'',NO.OF.REC,REC.ERR)
        Y.TELLER.ID = SEL.LIST<1>
        R.NEW(TT.TE.TELLER.ID.1) = Y.TELLER.ID
    END
RETURN
***************************************************************
END
*-----------------End of program--------------------------------------------------
