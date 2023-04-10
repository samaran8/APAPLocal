* @ValidationCode : MjotMTI4OTYwOTk0NTpDcDEyNTI6MTY4MTEyMjYyODQ2NTpIYXJpc2h2aWtyYW1DOi0xOi0xOjA6MDpmYWxzZTpOL0E6UjIxX0FNUi4wOi0xOi0x
* @ValidationInfo : Timestamp         : 10 Apr 2023 16:00:28
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
$PACKAGE APAP.REDOCHNLS

*-----------------------------------------------------------------------------
*MODIFICATION HISTORY:
*
* DATE              WHO                REFERENCE               DESCRIPTION
* 10-APR-2023     Conversion tool    R22 Auto conversion       No changes
* 10-APR-2023     Conversion tool    R22 Auto conversion       No changes
*-----------------------------------------------------------------------------
SUBROUTINE REDO.ARC.PDF.CREDIT(ROU.ARGS,ROU.RESPONSE,RESPONSE.TYPE,STYLE.SHEET)

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_ENQ.SES.VAR.CREDIT.COMMON

    RESPONSE.TYPE = 'XML.ENQUIRY'
    STYLE.SHEET = '/transforms/host/hostdetail.xsl'
    Y.LEN=LEN(PDF.CREDIT.DATA)
    Y.RES=PDF.CREDIT.DATA[0,Y.LEN-3]

    XML.RECS = '<window><panes><pane><dataSection><enqResponse>'
    XML.RECS :='<r><c><cap>' : PDF.CREDIT.HEADER : Y.RES : '^^PAGE=ededc.xsl </cap></c></r>'
    XML.RECS :='</enqResponse></dataSection></pane></panes></window>'
    ROU.RESPONSE = XML.RECS
RETURN
END
