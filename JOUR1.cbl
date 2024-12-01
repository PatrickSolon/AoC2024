       IDENTIFICATION DIVISION.
       PROGRAM-ID. JOUR1.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT InputFile ASSIGN TO
            "/mnt/c/pgmcobol/AoC2025/J1-1/input.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
          FILE SECTION.
            FD InputFile.
      *       Chaque ligne fait 13 caractères. 
              01 InputRecord PIC X(13).  
       
       WORKING-STORAGE SECTION.
         01 Table1.
           05 Table1-Data OCCURS 1000 TIMES INDEXED BY IDX1 PIC 9(05).  
         01 Table2.
           05 Table2-Data OCCURS 1000 TIMES INDEXED BY IDX2 PIC 9(05).
         01 RecordCounter PIC 9(04) VALUE 0.
         01 WS-Data1 PIC X(05).
         01 WS-Data1-num redefines WS-Data1 PIC 9(05).
         01 WS-Data2 PIC X(05).
         01 WS-Data2-num redefines WS-Data2 PIC 9(05).
         01 WS-EOF PIC A(1).
         01 WS-DIS PIC 9(5) VALUE 0.
         01 WS-DIS-TOTAL PIC 9(10) VALUE 0.
         01 WS-NB-SIM PIC 9(5) VALUE 0.
         01 WS-SIM PIC 9(15) VALUE 0.
         01 WS-SIM-TOTAL PIC 9(15) VALUE 0.

       PROCEDURE DIVISION.
       Main-Logic.
           MOVE 'N' TO WS-EOF
           OPEN INPUT InputFile
           PERFORM UNTIL WS-EOF = 'Y'
               READ InputFile INTO InputRecord
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       PERFORM Process-Record
           END-PERFORM
           CLOSE InputFile
       
           PERFORM Sort-Table1
           PERFORM Sort-Table2
       
           DISPLAY "Tri terminé."

           PERFORM distance 
            
           PERFORM similarite

           STOP RUN.
       
       Process-Record.
           ADD 1 TO RecordCounter
      *    Extraire les 5 premiers caractères. 
           MOVE InputRecord(1:5) TO WS-Data1 
      *    Extraire les 5 derniers caractères.         
           MOVE InputRecord(9:5) TO WS-Data2     
           MOVE WS-Data1-num TO Table1-Data(RecordCounter)
           MOVE WS-Data2-num TO Table2-Data(RecordCounter).
       
       Sort-Table1.
           SORT Table1-Data ASCENDING KEY Table1-Data.
       
       Sort-Table2.
           SORT Table2-Data ASCENDING KEY Table2-Data.
       
       distance.
           SET IDX1 IDX2 TO 1
           PERFORM UNTIL IDX1 > 1000
               if Table2-Data(IDX2) > Table1-Data(IDX1)
                  then
                    COMPUTE WS-DIS = Table2-Data(IDX2) 
                                     - Table1-Data(IDX1)
                  else
                    COMPUTE WS-DIS = Table1-Data(IDX1) 
                                     - Table2-Data(IDX2)
               end-if
               COMPUTE WS-DIS-TOTAL = WS-DIS-TOTAL + WS-DIS
               DISPLAY IDX1 ' - ' 
                       Table1-Data(IDX1) " - " Table2-Data(IDX2)
                       " - " WS-DIS ' - ' WS-DIS-TOTAL
               SET IDX1 IDX2 UP BY 1
           END-PERFORM

           DISPLAY "Total : " WS-DIS-TOTAL.

       similarite.
           SET IDX1  TO 1
           PERFORM UNTIL IDX1 > 1000
               SET IDX2 TO 1
               MOVE 0 TO WS-NB-SIM WS-SIM
               PERFORM UNTIL IDX2 > 1000
                  if Table1-Data(IDX1) = Table2-Data(IDX2)
                    then
                      ADD 1 TO WS-NB-SIM
                  end-if
                  SET IDX2 UP BY 1
               END-PERFORM 

               COMPUTE WS-SIM = Table1-Data(IDX1) * WS-NB-SIM
               COMPUTE WS-SIM-TOTAL = WS-SIM-TOTAL + WS-SIM
               SET IDX1 UP BY 1
           END-PERFORM

           DISPLAY "similarite : " WS-SIM-TOTAL.