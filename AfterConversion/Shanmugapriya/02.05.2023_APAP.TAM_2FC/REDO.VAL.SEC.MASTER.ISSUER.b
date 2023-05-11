* @ValidationCode : MjozMjU1NzM4ODk6Q3AxMjUyOjE2ODMwMDE1Mzc4ODg6SVRTUzotMTotMTowOjE6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 02 May 2023 09:55:37
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : ITSS
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : true
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_202108.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.TAM
SUBROUTINE REDO.VAL.SEC.MASTER.ISSUER
*------------------------------------------------------------------------------------------------------
*DESCRIPTION
* An input routine needs to be attached where the system can default the value from the field
* L.CU.RNC/L.CU.CIDENT from the CUSTOMER application when the ISSUER field is updated
*------------------------------------------------------------------------------------------------------
*APPLICATION
* SECURITY.MASTER
*-------------------------------------------------------------------------------------------------------

*
* Input / Output
* --------------
* IN     : -NA-
* OUT    : -NA-
*
*----------------------------------------------------------
* COMPANY NAME : APAP
* DEVELOPED BY : Temenos Application Management
* PROGRAM NAME : REDO.VAL.SEC.MASTER.ISSUER
*----------------------------------------------------------
* Modification History :
*-----------------------
*DATE             WHO         REFERENCE         DESCRIPTION
*23.08.2010      Janani     ODR-2011-02-0009   INITIAL CREATION
*
* ----------------------------------------------------------------------------
*Modification History
*DATE                       WHO                         REFERENCE                                   DESCRIPTION
*19-04-2023            Conversion Tool             R22 Auto Code conversion                      FM TO @FM VM TO @VM
*19-04-2023              Samaran T                R22 Manual Code conversion                         No Changes
*-----------------------------------------------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.CUSTOMER
    $INSERT I_F.SECURITY.MASTER

    GOSUB INIT
    GOSUB PROCESS
RETURN

*------------------------------------------------------------
INIT:
*------------------------------------------------------------
    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER = ''
    R.CUSTOMER = ''
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

    LF.APP = 'CUSTOMER':@FM:'SECURITY.MASTER'
    LF.FLD = 'L.CU.RNC':@VM:'L.CU.CIDENT':@FM:'ISSUER.TRN'
    LF.POS = ''

    CALL MULTI.GET.LOC.REF(LF.APP,LF.FLD,LF.POS)
    L.CU.RNC.POS = LF.POS<1,1>
    L.CU.CIDENT.POS = LF.POS<1,2>
    ISSUER.TRN.POS = LF.POS<2,1>
RETURN

*------------------------------------------------------------
PROCESS:
*------------------------------------------------------------

    ISSUER = R.NEW(SC.SCM.ISSUER)<1,1>
    CALL F.READ(FN.CUSTOMER,ISSUER,R.CUSTOMER,F.CUSTOMER,READ.ERR)
    L.CU.RNC = R.CUSTOMER<EB.CUS.LOCAL.REF,L.CU.RNC.POS>
    L.CU.CIDENT = R.CUSTOMER<EB.CUS.LOCAL.REF,L.CU.CIDENT.POS>

    BEGIN CASE

        CASE L.CU.RNC NE ''
            R.NEW(SC.SCM.LOCAL.REF)<1,ISSUER.TRN.POS> = L.CU.RNC
        CASE L.CU.CIDENT NE ''
            R.NEW(SC.SCM.LOCAL.REF)<1,ISSUER.TRN.POS> = L.CU.CIDENT
        CASE 1
            R.NEW(SC.SCM.LOCAL.REF)<1,ISSUER.TRN.POS> = ''
    END CASE

RETURN
*------------------------------------------------------------
END
