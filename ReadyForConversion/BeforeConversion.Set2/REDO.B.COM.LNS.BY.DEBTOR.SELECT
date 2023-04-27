*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
* <Rating>-10</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE REDO.B.COM.LNS.BY.DEBTOR.SELECT
*-----------------------------------------------------------------------------
*
* Description           : This is a Batch routine used to SELECT the all LENDING Arrangements

* Developed On          : 23-Sep-2013
*
* Developed By          : Amaravathi Krithika B
*
* Development Reference : DE11
*
*--------------------------------------------------------------------------------------------------
* Input Parameter:
* ---------------*
* Argument#1 : NA
*-----------------*
* Output Parameter:
* ----------------*
* Argument#2 : NA
*--------------------------------------------------------------------------------------------------
*  M O D I F I C A T I O N S
* ***************************
*--------------------------------------------------------------------------------------------------
* Defect Reference       Modified By                    Date of Change        Change Details
* (RTC/TUT/PACS)             NA                              NA                     NA
* PACS00382500           Ashokkumar.V.P                  06/03/2015      Added new fields based on mapping
* PACS00382500           Ashokkumar.V.P                  31/03/2015      Insert file compilation
*---------------------------------------------------------------------------------------------------
* Include files
*--------------------------------------------------------------------------------------------------
*
    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE
    $INSERT LAPAP.BP I_REDO.B.COM.LNS.BY.DEBTOR.COMMON


    GOSUB MAIN.PROCESS
    RETURN
*------------
MAIN.PROCESS:
**-----------
    CALL EB.CLEAR.FILE(FN.DR.REG.DE11.WORKFILE, F.DR.REG.DE11.WORKFILE)

    LIST.PARAMETER = ''
    LIST.PARAMETER<2> = "F.AA.ARRANGEMENT"
    LIST.PARAMETER<3> := "(PRODUCT.GROUP EQ ":"COMERCIAL":" OR PRODUCT.GROUP EQ ":"LINEAS.DE.CREDITO":")"
    LIST.PARAMETER<3> := " AND PRODUCT.LINE EQ ":"LENDING"
    CALL BATCH.BUILD.LIST(LIST.PARAMETER, "")
    RETURN
END
