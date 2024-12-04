       IDENTIFICATION DIVISION.
       PROGRAM-ID. JOUR4.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT InputFile ASSIGN TO
            "/mnt/c/pgmcobol/AoC2024/J4/input.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
          FILE SECTION.
            FD InputFile.
      *       Chaque ligne fait 140 caractères. 
              01 InputRecord PIC X(140).  
       
       WORKING-STORAGE SECTION.
       01  TABLEAU.
           05  LIGNE OCCURS 140.
               10  COLONNE OCCURS 140.
                   15 CARACT PIC X(1).

       01 RecordCounter PIC 9(04) VALUE 0.
       01  i                        pic 9(6) value ZERO.
       01  j                        pic 9(6) value ZERO.
       01  k                        pic 9(6) value ZERO.
       01  COMPTEUR-OCCURENCES      PIC 9(6) VALUE ZERO.
       01  ws-txt-chrch             pic x(3) value spaces.
       01  ws-pivot                 pic x(1) value space.

       01  WS-EOF PIC A(1).
       
       01  ws-max-i pic 9(3) value 0.
       01  ws-max-j pic 9(3) value 0.

       01  ws-nb-x pic 9(3) value 0.

       01  WS-diagonale   PIC 9 VALUE 0.
           88  diag-ok          VALUE 1.
           88  diag-ko          VALUE 0.

       PROCEDURE DIVISION.
       Main-Logic.

           perform traitement-fichier
           
           move 140 to ws-max-i ws-max-j

           perform varying i from 1 by 1 until i > ws-max-i
              perform varying j from 1 by 1 until j > ws-max-j
                 display caract(i,j) WITH NO ADVANCING
              end-perform
              display " "
           end-perform

           move 0 to COMPTEUR-OCCURENCES 
           move 0 To ws-nb-x

           move 'MAS' to ws-txt-chrch
           perform recherche-chaine

      *     move 'SAMX' to ws-txt-chrch
      *     perform recherche-chaine

           display "nombre d'occurence : " COMPTEUR-OCCURENCES 
 
           STOP RUN.
       
       traitement-fichier.
           MOVE 'N' TO WS-EOF
           OPEN INPUT InputFile
           PERFORM UNTIL WS-EOF = 'Y'
               READ InputFile INTO InputRecord
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       PERFORM Process-Record
               END-READ
           END-PERFORM
           CLOSE InputFile
           .

       Process-Record.
           ADD 1 TO RecordCounter
           MOVE InputRecord TO LIGNE(RecordCounter) 
           .
       
       recherche-chaine.
           move ws-txt-chrch(2:1) to ws-pivot
           perform varying i from 1 by 1 until i > ws-max-i
              perform varying j from 1 by 1 until j > ws-max-j
                 if caract(i,j) = ws-pivot
                    add 1 to ws-nb-x
                    display "traitement du A N°" ws-nb-x 
                          " emplacement : " i "," j
                    move 'MAS' to ws-txt-chrch
                    set diag-ko to true
                    perform recherche-diag-1
                    if diag-ko
                       move 'SAM' to ws-txt-chrch
                       perform recherche-diag-1
                    end-if
                    if diag-ok
                        move 'MAS' to ws-txt-chrch
                        set diag-ko to true
                        perform recherche-diag-2
                        if diag-ko
                            move 'SAM' to ws-txt-chrch
                            perform recherche-diag-2
                        end-if
                    end-if 
                    if diag-ok
                       add 1 to COMPTEUR-OCCURENCES
                    end-if
                 end-if          
              end-perform
           end-perform
           .
           
       recherche-diag-1.
           if (j > 1 and j < 140) and (i > 1 and i < 140)
              if caract(i,j) = ws-txt-chrch(2:1)
                 and caract(i - 1,j + 1 ) = ws-txt-chrch(1:1)
                 and caract(i + 1,j - 1 ) = ws-txt-chrch(3:1)
                 display ws-txt-chrch " en position DFH : " i "," j
                 set diag-ok to true 
              end-if
          END-IF
           .

       recherche-diag-2.
           if (j > 1 and j < 140) and (i > 1 and i < 140)
              if caract(i,j) = ws-txt-chrch(2:1)
                 and caract(i - 1,j - 1 ) = ws-txt-chrch(1:1)
                 and caract(i + 1,j + 1 ) = ws-txt-chrch(3:1)
                 display ws-txt-chrch " en position dfb : " i "," j
                 set diag-ok to true 
              end-if
           END-IF
           .