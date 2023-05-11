* @ValidationCode : MjoxMDMxNjMzMjU3OkNwMTI1MjoxNjgwNjk3NTMzMzIzOkhhcmlzaHZpa3JhbUM6LTE6LTE6MDowOmZhbHNlOk4vQTpSMjFfQU1SLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 05 Apr 2023 17:55:33
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : HarishvikramC
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : R21_AMR.0
* @ValidationInfo : Copyright Temenos Headquarters SA 1993-2021. All rights reserved.
$PACKAGE APAP.REDOBATCH
SUBROUTINE REDO.B.RENEW.PROCESS.LOAD
*-------------------------------------------------------------------------------------------
*DESCRIPTION:
*             This load routine initialises and opens necessary files
*  and gets the position of the local reference fields
* ------------------------------------------------------------------------------------------
* Input/Output:
*--------------
* IN  : -NA-
* OUT : -NA-
*
* Dependencies:
*---------------
* CALLS     : -NA-
* CALLED BY : -NA-
*
* Revision History:
*------------------
*   Date               who             Reference            Description
* 07-AUG-2010     S.R.SWAMINATHAN   ODR-2010-03-0400      Initial Creation
* 04-APR-2023     Conversion tool   R22 Auto conversion   VM to @VM
* 04-APR-2023      Harishvikram C   Manual R22 conversion      No changes
*---------------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_REDO.B.RENEW.PROCESS.COMMON
    $INSERT I_F.LATAM.CARD.ORDER
    $INSERT I_F.REDO.CARD.REQUEST
    $INSERT I_BATCH.FILES
    $INSERT I_F.COMPANY
    $INSERT I_GTS.COMMON
    $INSERT I_F.CARD.TYPE
    $INSERT I_F.DATES
*   $INSERT I_F.COMPANY ;*R22 Auto conversion
    $INSERT I_F.CUSTOMER
    $INSERT I_F.REDO.CARD.RENEWAL

    GOSUB INIT
    GOSUB GET.LR.FLD.POS
    GOSUB OPEN.FILE

RETURN

*----
INIT:
*----
*-------------------------------------------------
* This section initialises the necessary variables
*-------------------------------------------------

    FN.REDO.CARD.REQUEST = 'F.REDO.CARD.REQUEST'
    F.REDO.CARD.REQUEST = ''

    FN.LATAM.CARD.ORDER = 'F.LATAM.CARD.ORDER'
    F.LATAM.CARD.ORDER = ''

    FN.CARD.TYPE = 'F.CARD.TYPE'
    F.CARD.TYPE = ''

    FN.DATES = 'F.DATES'
    F.DATES = ''

    FN.CARD.RENEW = 'F.REDO.CARD.RENEWAL'
    F.CARD.RENEW = ''
    CALL OPF(FN.CARD.RENEW,F.CARD.RENEW)

    FN.COMPANY = 'F.COMPANY'
    F.COMPANY = ''

    FN.CUSTOMER = 'F.CUSTOMER'
    F.CUSTOMER =''


RETURN

*--------------
GET.LR.FLD.POS:
*--------------
*-------------------------------------------------------------
* This section gets the position of the local reference fields
*-------------------------------------------------------------

    Y.LRF.APPL = "CARD.TYPE"
    Y.LRF.FIELDS = 'L.CT.RENEW.PROC':@VM:'L.CT.BIN'
    FIELD.POS = ''
    CALL MULTI.GET.LOC.REF(Y.LRF.APPL,Y.LRF.FIELDS,FIELD.POS)
    Y.CT.RENEW.PROC.POS = FIELD.POS<1,1>
    Y.CT.BIN.POS = FIELD.POS<1,2>
RETURN

*---------
OPEN.FILE:
*---------
*---------------------------------------
* This section opens the necessary files
*---------------------------------------

    CALL OPF(FN.REDO.CARD.REQUEST,F.REDO.CARD.REQUEST)
    CALL OPF(FN.CARD.TYPE,F.CARD.TYPE)
    CALL OPF(FN.LATAM.CARD.ORDER,F.LATAM.CARD.ORDER)
    CALL OPF(FN.DATES,F.DATES)
    CALL OPF(FN.COMPANY,F.COMPANY)
    CALL OPF(FN.CUSTOMER,F.CUSTOMER)

RETURN

END
