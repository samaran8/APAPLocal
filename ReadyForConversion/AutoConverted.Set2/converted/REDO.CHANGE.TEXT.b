SUBROUTINE REDO.CHANGE.TEXT

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON

* Developed by: TAM (marimuthu s)
* Reference : PACS00245134
* Description : This is conversion routine to translate english to spanish


    BEGIN CASE

        CASE O.DATA EQ 'Processing...'
            O.DATA = 'En Proceso...'
        CASE O.DATA EQ 'Executed - Successfully'
            O.DATA = 'Proceso Completado'
        CASE O.DATA EQ 'Completed - Error'
            O.DATA = 'Error en Proceso '
    END CASE

RETURN

END
