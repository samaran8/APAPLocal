* @ValidationCode : MjoyNzIzMzE1NDU6Q3AxMjUyOjE2ODA2MDk4MDY3NzA6SVRTU0JORzotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDIxMDguMDotMTotMQ==
* @ValidationInfo : Timestamp         : 04 Apr 2023 17:33:26
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
******************************************************************************
SUBROUTINE REDO.FC.GET.VERSION
******************************************************************************
* Company Name:   Asociacion Popular de Ahorro y Prestamo (APAP)
* Developed By:   Reginal Temenos Application Management
*-----------------------------------------------------------------------------
* Subroutine Type :  BUILD ROUTINE
* Attached to     :  ENQUIRY > REDO.FC.E.PENDING.AA
* Attached as     :  CONVERSION
* Primary Purpose :  Permite invocar a una version especifica de RCA
*                    de acuerdo al tipo de producto
*
* Incoming        :  NA
* Outgoing        :  NA
*
*-----------------------------------------------------------------------------
* Modification History:
* ====================
* Development by  : lpazminodiaz@temenos.com
* Date            : 09/09/2011
* Purpose         : Initial version
* Modification History:
* Date                 Who                              Reference                            DESCRIPTION
*04-04-2023           CONVERSION TOOL                AUTO R22 CODE CONVERSION                 NO CHANGES
*04-04-2023          jayasurya H                       MANUAL R22 CODE CONVERSION            NO CHANGES
******************************************************************************

******************************************************************************

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.AA.ARRANGEMENT

    $INSERT I_F.REDO.FC.ACT.PRES

    GOSUB INITIALISE
    GOSUB OPEN.FILES
    GOSUB PROCESS

RETURN

* ========
INITIALISE:
* ========
    FN.REDO.FC.ACT.PRES = 'F.REDO.FC.ACT.PRES'
    F.REDO.FC.ACT.PRES = ''
    R.REDO.FC.ACT.PRES = ''

    Y.ERR = ''

RETURN

* ========
OPEN.FILES:
* ========
    CALL OPF(FN.REDO.FC.ACT.PRES,F.REDO.FC.ACT.PRES)

RETURN

* =====
PROCESS:
* =====
    Y.PRODUCT.ID = R.RECORD<AA.ARR.PRODUCT>
    CALL CACHE.READ(FN.REDO.FC.ACT.PRES,Y.PRODUCT.ID,R.REDO.FC.ACT.PRES,Y.ERR)
    Y.VERSION = R.REDO.FC.ACT.PRES<REDO.FC.ACT.DESC.PRODUCT>

    O.DATA = Y.VERSION : ' SEE '

RETURN

END
