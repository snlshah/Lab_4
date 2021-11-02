       IDENTIFICATION DIVISION.
       PROGRAM-ID. TOTAL5.
       AUTHOR. SONALI SHAH.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT COURSE-FILE ASSIGN TO 'DA-S-COURSE'.
       SELECT PRINT-FILE ASSIGN TO 'UR-S-PRINT'.
       DATA DIVISION.
       FILE SECTION.
       FD COURSE-FILE
       RECORDING MODE IS F
       LABEL RECORDS ARE STANDARD.
       01 COURSE-REC PIC X(80).
       FD PRINT-FILE
       RECORDING MODE IS F
       LABEL RECORDS ARE STANDARD.
       01 PRINT-REC PIC X(132).
       WORKING-STORAGE SECTION.
       01 MISC.

        03 EOF PIC X VALUE 'N'.
        88 END-OF-DATA VALUE 'Y'.
        03 PAGEKOUNT PIC 99 VALUE 01.

        03 LINE-CT PIC 99 VALUE 0.
        03 TSEATSR PIC 99 VALUE 0.
        03 TSEATSL PIC 99 VALUE 0.
        03 TSEATST PIC 99 VALUE 0.    
        03 SEATSTAKEN PIC 999 VALUE 0.    
       01 COURSE-DATA.
        03 C-COURSE.
         05 C-ABB PIC XXX.
         05 C-NUMB PIC XXXX.
         05 C-SEC PIC XXX.
        03 C-TITLE PIC X(20).
        03 C-SEATS-REMAINING PIC 999.
        03 C-CLASSLIMIT PIC 999.
        03 FILLER PIC XXX.
        03 C-STARTING-TIME.
         05 C-STARTING-HOUR PIC 99.
         05 C-STARTING-MIN PIC 99.
        03 FILLER PIC XX.
        03 C-DAYS PIC X(6).
        03 C-LOCATION.
         05 C-BUILDING PIC XX.
         05 C-ROOM PIC XXX.
        03 FILLER PIC X(24). 
            01 HEADING1.
        03 FILLER PIC X(10) VALUE SPACES.
        03 FILLER PIC X(5) VALUE 'CLASS'.
        03 FILLER PIC X(11) VALUE SPACES.
        03 FILLER PIC X(8) VALUE 'LOCATION'.
        03 FILLER PIC X(8) VALUE SPACES.
        03 FILLER PIC X(4) VALUE 'DAYS'.
        03 FILLER PIC X(11) VALUE SPACES.
        03 FILLER PIC X(4) VALUE 'TIME'.
        03 FILLER PIC X(10) VALUE SPACES.
        03 FILLER PIC X(5) VALUE 'CLASS'.
        03 FILLER PIC X(7) VALUE SPACES.
        03 FILLER PIC XXXX VALUE 'OPEN'.
        03 FILLER PIC X(3) VALUE SPACES.
        03 FILLER PIC X(5) VALUE 'TAKEN'.    
       01 HEADING2.
        03 FILLER PIC X(71) VALUE SPACES.
        03 FILLER PIC X(5) VALUE 'LIMIT'.
        03 FILLER PIC X(7) VALUE SPACES.
        03 FILLER PIC X(5) VALUE 'SEATS'.
       01 HEADING3.
        03 FILLER PIC X(10) VALUE SPACES.
        03 FILLER PIC X(11) VALUE 'GRAND TOTAL'.
        03 FILLER PIC X(51) VALUE SPACES.
        03 TSEATSLL PICTURE 999.
        03 FILLER PIC X(9) VALUE SPACES.
        03 TSEATSRR PICTURE 99.
        03 FILLER PIC X(5) VALUE SPACES.
        03 TSEATSTT PICTURE 999.   
       01 HEADING4.
        03 FILLER PIC X(30) VALUE SPACES.
        03 FILLER PIC X(27) VALUE 'EASTERN ILLINOIS UNIVERSITY'.
        03 FILLER PIC X(30) VALUE SPACES.
        03 FILLER PIC X(5) VALUE 'PAGE:'.
        03 PAGEK PICTURE ZZ.
       01 HEADING5.
        03 FILLER PIC X(37) VALUE SPACES.
        03 FILLER PIC X(14) VALUE 'COURSE LISTING'.     
       01 PRINT-DATA.
        03 FILLER PIC X(10) VALUE SPACES.
        03 PABB PIC XXX.
        03 FILLER PIC X VALUE SPACES.
        03 PNUMB PIC XXXX.
        03 FILLER PIC X VALUE SPACES.
        03 PSEC PIC XXX. 
        03 FILLER PIC X(5) VALUE SPACES.
        03 PBUILDING PIC XX.
        03 FILLER PIC X VALUE SPACES.
        03 PROOM PIC XXX.
        03 FILLER PIC X(9) VALUE SPACES.
        03 PDAYS PIC X(6).
        03 FILLER PIC X(10) VALUE SPACES.
        03 PSTARTING-HOUR PIC Z9.
        03 FILLER PIC X VALUE ':'.
        03 PSTARTING-MIN PIC 99.
        03 FILLER PIC X(9) VALUE SPACES.
        03 PCLASSLIMIT PIC ZZ9.
        03 FILLER PIC X(8) VALUE SPACES.
        03 PSEATS-REMAINING PIC ZZ9.
        03 FILLER PIC X(4) VALUE SPACES.
        03 PSEATS-TAKEN PIC ZZ9.
       PROCEDURE DIVISION.
       000-MAINLINE.
           OPEN INPUT COURSE-FILE
       OUTPUT PRINT-FILE.
           PERFORM 800-READ-COURSE-FILE.
           
           PERFORM 225-COURSE-HEADINGS.
           
       PERFORM 100-PROCESS-LOOP
           UNTIL END-OF-DATA.
           
           PERFORM 700-TOTAL.

       CLOSE COURSE-FILE
       PRINT-FILE.
       STOP RUN.
         
       100-PROCESS-LOOP.
       IF LINE-CT > 45
           THEN
       
                   PERFORM 225-COURSE-HEADINGS.
      
      
       MOVE C-ABB TO PABB.
       MOVE C-NUMB TO PNUMB.
       MOVE C-SEC TO PSEC.
       MOVE C-BUILDING TO PBUILDING.
       MOVE C-ROOM TO PROOM.
        IF C-DAYS = 'M W F'
                MOVE 'M-W-F' TO PDAYS
           ELSE MOVE '-T-H-' TO PDAYS.
      
       MOVE C-STARTING-HOUR TO PSTARTING-HOUR.
       MOVE C-STARTING-MIN TO PSTARTING-MIN.
       MOVE C-SEATS-REMAINING TO PSEATS-REMAINING.
           MOVE C-CLASSLIMIT TO PCLASSLIMIT.   
           PERFORM 880-TAKEN. 
           PERFORM 885-TAKENTOTAL.
           PERFORM 900-LIMIT.
           PERFORM 920-REMAINING.
           
           MOVE PAGEKOUNT TO PAGEK.
           WRITE PRINT-REC FROM PRINT-DATA
       AFTER ADVANCING 1 LINE.
           ADD 1 TO LINE-CT. 
          
          
           PERFORM 800-READ-COURSE-FILE.
        880-TAKEN.
           MOVE PSEATS-REMAINING TO TSEATSR.
           MOVE PCLASSLIMIT TO TSEATSL.
          COMPUTE PSEATS-TAKEN = TSEATSL - TSEATSR.

       885-TAKENTOTAL.
           MOVE PSEATS-TAKEN TO TSEATST.
           ADD TSEATST TO TSEATSTT.

       920-REMAINING.
           MOVE PSEATS-REMAINING TO TSEATSR.
           ADD TSEATSR TO TSEATSRR.
           
       900-LIMIT.
           MOVE PCLASSLIMIT TO TSEATSL.
           ADD TSEATSL TO TSEATSLL.
   
       700-TOTAL.      
           WRITE PRINT-REC FROM HEADING3
           AFTER ADVANCING 2 LINES.
             
           
       225-COURSE-HEADINGS.
           MOVE PAGEKOUNT TO PAGEK.
           
           WRITE PRINT-REC FROM HEADING4
           AFTER ADVANCING 1 LINE.
           WRITE PRINT-REC FROM HEADING5
           AFTER ADVANCING 2 LINES.
           WRITE PRINT-REC FROM HEADING1
           AFTER ADVANCING 2 LINES.
           WRITE PRINT-REC FROM HEADING2
           AFTER ADVANCING 2 LINES.
           MOVE SPACES TO PRINT-REC.
           WRITE PRINT-REC
           AFTER ADVANCING 1.      
           MOVE 0 TO LINE-CT.
     
       800-READ-COURSE-FILE.
           READ COURSE-FILE INTO COURSE-DATA
           AT END MOVE 'Y' TO EOF.
