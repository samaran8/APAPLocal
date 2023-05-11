*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE DR.REGN16.GET.OPEN.DATE
*-------------------------------------------------------------------------
* Date              Author                    Description
* ==========        ====================      ============
* 18-06-2015        Ashokkumar                PACS00465380- Initial revision
*-------------------------------------------------------------------------
* Routine was resend for the fix PACS00305215

    $INSERT T24.BP I_COMMON
    $INSERT T24.BP I_EQUATE

    FORM.OPEN.DATE = ''
    OPEN.DATE = COMI
    IF OPEN.DATE THEN
        FORM.OPEN.DATE = OPEN.DATE[7,2]:'/':OPEN.DATE[5,2]:'/':OPEN.DATE[1,4]
    END
    COMI = FORM.OPEN.DATE

    RETURN
END
