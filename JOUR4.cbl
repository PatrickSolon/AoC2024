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
       01  ws-txt-chrch             pic x(4) value spaces.
       01  ws-pivot                 pic x(1) value space.

       01  WS-EOF PIC A(1).
       
       01  ws-max-i pic 9(3) value 0.
       01  ws-max-j pic 9(3) value 0.

       01  ws-nb-x pic 9(3) value 0.

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

           move 'XMAS' to ws-txt-chrch
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
           move ws-txt-chrch(1:1) to ws-pivot
           perform varying i from 1 by 1 until i > ws-max-i
              perform varying j from 1 by 1 until j > ws-max-j
                 if caract(i,j) = ws-pivot
                    add 1 to ws-nb-x
                    display "traitement du x N°" ws-nb-x 
                          " emplacement : " i "," j 
                    perform recherche-front
                    perform recherche-back
                    perform recherche-haut
                    perform recherche-bas
                    perform recherche-dfh
                    perform recherche-dfb
                    perform recherche-dbh
                    perform recherche-dbb                    
              end-perform
           end-perform
           .
           
       recherche-front.
           if j < 138
              if caract(i,j) = ws-txt-chrch(1:1)
                 and caract(i,j + 1 ) = ws-txt-chrch(2:1)
                 and caract(i,j + 2 ) = ws-txt-chrch(3:1)
                 and caract(i,j + 3 ) = ws-txt-chrch(4:1)
                 display "XMAS en position avant : " i "," j 
                 add 1 to COMPTEUR-OCCURENCES
              end-if
           END-IF
           .

       recherche-back.
           if j > 3
              if caract(i,j) = ws-txt-chrch(1:1)
                 and caract(i,j - 1 ) = ws-txt-chrch(2:1)
                 and caract(i,j - 2 ) = ws-txt-chrch(3:1)
                 and caract(i,j - 3 ) = ws-txt-chrch(4:1)
                 display "XMAS en position arrière : " i "," j 
                 add 1 to COMPTEUR-OCCURENCES
              end-if
           END-IF
           .

       recherche-haut.
           if i > 3
              if caract(i,j) = ws-txt-chrch(1:1)
                 and caract(i - 1,j) = ws-txt-chrch(2:1)
                 and caract(i - 2,j) = ws-txt-chrch(3:1)
                 and caract(i - 3,j) = ws-txt-chrch(4:1)
                 add 1 to COMPTEUR-OCCURENCES
                 display "XMAS en position haute : " i "," j 
              end-if
           END-IF
           .

       recherche-bas.
           if i < 138
              if caract(i,j) = ws-txt-chrch(1:1)
                 and caract(i + 1,j) = ws-txt-chrch(2:1)
                 and caract(i + 2,j) = ws-txt-chrch(3:1)
                 and caract(i + 3,j) = ws-txt-chrch(4:1)
                 add 1 to COMPTEUR-OCCURENCES
                 display "XMAS en position basse : " i "," j 
              end-if
           END-IF
           .

       recherche-dfh.
           if j < 138 and i > 3
              if caract(i,j) = ws-txt-chrch(1:1)
                 and caract(i - 1,j + 1 ) = ws-txt-chrch(2:1)
                 and caract(i - 2,j + 2 ) = ws-txt-chrch(3:1)
                 and caract(i - 3,j + 3 ) = ws-txt-chrch(4:1)
                 add 1 to COMPTEUR-OCCURENCES
                 display "XMAS en position DFH : " i "," j 
              end-if
           END-IF
           .

       recherche-dfb.
           if j < 138 and i < 138
              if caract(i,j) = ws-txt-chrch(1:1)
                 and caract(i + 1,j + 1 ) = ws-txt-chrch(2:1)
                 and caract(i + 2,j + 2 ) = ws-txt-chrch(3:1)
                 and caract(i + 3,j + 3 ) = ws-txt-chrch(4:1)
                 add 1 to COMPTEUR-OCCURENCES
                 display "XMAS en position dfb : " i "," j 
              end-if
           END-IF
           .

       recherche-dbh.
           if j > 3 and i > 3
              if caract(i,j) = ws-txt-chrch(1:1)
                 and caract(i - 1,j - 1 ) = ws-txt-chrch(2:1)
                 and caract(i - 2,j - 2 ) = ws-txt-chrch(3:1)
                 and caract(i - 3,j - 3 ) = ws-txt-chrch(4:1)
                 add 1 to COMPTEUR-OCCURENCES
                 display "XMAS en position dbh : " i "," j 
              end-if
           END-IF
           .

       recherche-dbb.
           if j > 3 and i < 138
              if caract(i,j) = ws-txt-chrch(1:1)
                 and caract(i + 1,j - 1 ) = ws-txt-chrch(2:1)
                 and caract(i + 2,j - 2 ) = ws-txt-chrch(3:1)
                 and caract(i + 3,j - 3 ) = ws-txt-chrch(4:1)
                 add 1 to COMPTEUR-OCCURENCES
                 display "XMAS en position dbb : " i "," j 
              end-if
           END-IF
           .