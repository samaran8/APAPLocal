* @ValidationCode : MjotMTczNDc4MTcyOkNwMTI1MjoxNjgyNjczNjM5NDI2OklUU1M6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 28 Apr 2023 14:50:39
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
$PACKAGE APAP.REDOENQ
*-----------------------------------------------------------------------------
* <Rating>-69</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE REDO.E.FT.TT.LIST(ENQ.DATA)
*-----------------------------------------------------------------------------
* Company Name : ASOCIACION POPULAR DE AHORROS Y PRESTAMOS
* Developed By : Pradeep M
* Program Name : REDO.E.FT.TT.LIST
*-----------------------------------------------------------------------------
* Description :Bulit routine to assign value to set variable
* Linked with :
* In Parameter :
* Out Parameter :
*-----------------------------------------------------------------------------
* DATE         DEVELOPER             ODR              VERSION
* 10-11-2011   Pradeep M             ODR2011080055    INITIAL VERSION
* 27-06-2013   Vignesh Kumaar M R    PACS00296955     INCLUDE TFS RECORDS IN THE LIST
* 10-07-2013   Vignesh Kumaar M R    PACS00306457     DISCARD THE TT THAT ARE CREATED FOR TFS
* 10-03-2014   Vignesh Kumaar M R    PACS00349444     CAN BE AMENDED ONLY USING L.ACTUAL.VERSIO
* 28-APRIL-2023      Conversion Tool       R22 Auto Conversion -  FM to @FM
* 28-APRIL-2023      Harsha                R22 Manual Conversion - No changes
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON

    $INSERT I_F.TELLER
    $INSERT I_F.FUNDS.TRANSFER
    $INSERT I_F.T24.FUND.SERVICES ;*

    GOSUB OPEN.PROCESS
    GOSUB PROCESS

RETURN

OPEN.PROCESS:
*-----------

    FN.TELLER='F.TELLER'
    F.TELLER=''
    CALL OPF(FN.TELLER,F.TELLER)

    FN.FUNDS.TRANSFER='F.FUNDS.TRANSFER'
    F.FUNDS.TRANSFER=''
    CALL OPF(FN.FUNDS.TRANSFER,F.FUNDS.TRANSFER)

* Fix for PACS00296955 [INCLUDE TFS RECORDS IN THE LIST #1]

    FN.T24.FUND.SERVICES = 'F.T24.FUND.SERVICES'
    F.T24.FUND.SERVICES = ''
    CALL OPF(FN.T24.FUND.SERVICES,F.T24.FUND.SERVICES)

* End of Fix

* Fix for PACS00349444 [CAN BE AMENDED ONLY USING L.ACTUAL.VERSIO #1]

    Y.APPLICATION = 'FUNDS.TRANSFER':@FM:'TELLER':@FM:'T24.FUND.SERVICES'
    Y.FIELDS = 'L.ACTUAL.VERSIO':@FM:'L.ACTUAL.VERSIO':@FM:'L.T24FS.TRA.DAY'
    Y.POS = ''

    CALL MULTI.GET.LOC.REF(Y.APPLICATION,Y.FIELDS,Y.POS)
    Y.FT.POS = Y.POS<1,1>
    Y.TT.POS = Y.POS<2,1>
    Y.TFS.POS = Y.POS<3,1>

* End of Fix

RETURN

PROCESS:
*-------

    Y.USER.ID = OPERATOR

    GOSUB GET.FT.LIST
    GOSUB GET.TT.LIST
    GOSUB GET.TFS.LIST

    ENQ.DATA = Y.DATA

RETURN

GET.FT.LIST:
*----------*

    SEL.LIST.FT = ''
    NO.OF.REC = ''
    ERR.FT = ''

*    SEL.CMD.FT = "SELECT ":FN.FUNDS.TRANSFER: " WITH INPUTTER LIKE ...":Y.USER.ID:"..."
    SEL.CMD.FT = "SELECT ":FN.FUNDS.TRANSFER: " WITH L.INP.USER.ID EQ ":Y.USER.ID
    CALL EB.READLIST(SEL.CMD.FT,SEL.LIST.FT,'',NO.OF.REC,ERR.FT)

* Fix for PACS00349444 [CAN BE AMENDED ONLY USING L.ACTUAL.VERSIO #2]

    LOOP
        REMOVE FT.ID FROM SEL.LIST.FT SETTING FT.POS
    WHILE FT.ID:FT.POS
        CALL F.READ(FN.FUNDS.TRANSFER,FT.ID,R.FUNDS.TRANSFER,F.FUNDS.TRANSFER,FUNDS.TRANSFER.ERR)
        Y.ACTUAL.VERSION = R.FUNDS.TRANSFER<FT.LOCAL.REF,Y.FT.POS>
        Y.DATA<-1> = FT.ID:'*':Y.ACTUAL.VERSION
    REPEAT

* End of Fix

RETURN


GET.TT.LIST:
*----------*

    SEL.LIST.TT = ''
    NO.OF.REC = ''
    ERR.TT = ''

*    SEL.CMD.TT = "SELECT ":FN.TELLER: " WITH INPUTTER LIKE ...":Y.USER.ID:"... AND T24.FS.REF UNLIKE T24FS..."          ;* Fix for PACS00306457
    SEL.CMD.TT = "SELECT ":FN.TELLER: " WITH L.INP.USER.ID EQ ":Y.USER.ID:" AND T24.FS.REF UNLIKE T24FS..."
    CALL EB.READLIST(SEL.CMD.TT,SEL.LIST.TT,'',NO.OF.REC,ERR.TT)

* Fix for PACS00349444 [CAN BE AMENDED ONLY USING L.ACTUAL.VERSIO #3]

    LOOP
        REMOVE TT.ID FROM SEL.LIST.TT SETTING TT.POS
    WHILE TT.ID:TT.POS
        CALL F.READ(FN.TELLER,TT.ID,R.TELLER,F.TELLER,TELLER.ERR)
        Y.ACTUAL.VERSION = R.TELLER<TT.TE.LOCAL.REF,Y.TT.POS>
        Y.DATA<-1> = TT.ID:'*':Y.ACTUAL.VERSION
    REPEAT

* End of Fix

RETURN


GET.TFS.LIST:
*-----------*

* Fix for PACS00296955 [INCLUDE TFS RECORDS IN THE LIST #2]

    SEL.LIST.TFS = ''
    NO.OF.REC = ''
    ERR.TFS = '' ;

*    SEL.CMD.TFS = "SELECT ":FN.T24.FUND.SERVICES: " WITH INPUTTER LIKE ...":Y.USER.ID:"..."
    SEL.CMD.TFS = "SELECT ":FN.T24.FUND.SERVICES: " WITH L.INP.USER.ID EQ ":Y.USER.ID
    CALL EB.READLIST(SEL.CMD.TFS,SEL.LIST.TFS,'',NO.OF.REC,ERR.TFS)

* Fix for PACS00349444 [CAN BE AMENDED ONLY USING L.ACTUAL.VERSIO #4]

    LOOP
        REMOVE TFS.ID FROM SEL.LIST.TFS SETTING TFS.POS
    WHILE TFS.ID:TFS.POS
        CALL F.READ(FN.T24.FUND.SERVICES,TFS.ID,R.T24.FUND.SERVICES,F.T24.FUND.SERVICES,TFS.ERR)
        Y.ACTUAL.VERSION = R.T24.FUND.SERVICES<TFS.LOCAL.REF,Y.TFS.POS>
        Y.DATA<-1> = TFS.ID:'*':Y.ACTUAL.VERSION
    REPEAT

* End of Fix

RETURN
END
