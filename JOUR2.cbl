       IDENTIFICATION DIVISION.
       PROGRAM-ID. LIRE-FICHIER-TABLEAU.

      * SOURCE-COMPUTER. IBM-370 WITH DEBUGGING MODE. 
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FICHIER-ENTREE ASSIGN 
               TO '/mnt/c/pgmcobol/AoC2024/J2/input.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  FICHIER-ENTREE.
       01  LIGNE-FICHIER PIC X(100).

       WORKING-STORAGE SECTION.
      *> Max lignes du tableau 
       01  MAX-LIGNES       PIC 9(4) VALUE 1000.  
      *> Max colonnes du tableau
       01  MAX-COLONNES     PIC 9(4) VALUE 10.  
       01  NOMBRE-LIGNES    PIC 9(4) VALUE 0.
       01  TABLEAU.
           05  LIGNE OCCURS 1 TO 1000 DEPENDING ON NOMBRE-LIGNES.
               10  COLONNE OCCURS 10 PIC 9(2).
       01  TABLEAU-DUMPER.
           05  COLONNE-DUMP OCCURS 10 PIC 9(2).
       01  INDEX-LIGNE      PIC 9(4) VALUE 0.
       01  INDEX-COLONNE    PIC 9(4) VALUE 0.
       01  INDEX-COLONNE-DUMP    PIC 9(4) VALUE 0.
       01  VALEUR           PIC 9(2).
       01  IND-COL-INF      PIC 9(2).
       01  ws-diff          PIC 9(2).
       01  WS-NB-PROB       PIC 9(2).
       01  index-a-supp     pic 9(2).

       01  WS-NB-RAPPORTS-OK PIC 9(4) VALUE 0.
       01  WS-NB-RAPPORTS-OK-DUMP PIC 9(4) VALUE 0.
       

       01  WS-FICHIER   PIC 9 VALUE 0.
           88  FIN-FICHIER       VALUE 1.
           88  NOT-FIN-FICHIER   VALUE 0.

       01  WS-RAPPORT   PIC 9 VALUE 0.
           88  RAPPORT-ASC       VALUE 1.
           88  RAPPORT-DESC      VALUE 0.

       01  WS-RAPPORT-OKKO   PIC 9 VALUE 0.
           88  RAPPORT-OK       VALUE 1.
           88  RAPPORT-KO       VALUE 0.

       01  WS-RAPPORT-DUMPER         PIC 9 VALUE 0.
           88  RAPPORT-OK-DUMP       VALUE 1.
           88  RAPPORT-KO-DUMP       VALUE 0.

       01  WS-TRAITEMENT-DUMPER         PIC 9 VALUE 0.
           88  FIN-TRAIT-DUMP-OK       VALUE 1.
           88  FIN-TRAIT-DUMP-KO       VALUE 0.

       PROCEDURE DIVISION.
       DEBUT.
           OPEN INPUT FICHIER-ENTREE.
           PERFORM LIRE-FICHIER-JUSQUA-FIN.
           CLOSE FICHIER-ENTREE.
      D    PERFORM AFFICHER-TABLEAU.
      D    PERFORM AFFICHER-LIGNE.
           PERFORM TRAITER-TABLEAU.
           DISPLAY 'Nombre de rapports valide : ' WS-NB-RAPPORTS-OK
           DISPLAY 'Nombre de rapports valide avec DUMPER : '
                    WS-NB-RAPPORTS-OK-DUMP
           STOP RUN.

       LIRE-FICHIER-JUSQUA-FIN.
           PERFORM UNTIL FIN-FICHIER
           READ FICHIER-ENTREE INTO LIGNE-FICHIER
               AT END
      D            DISPLAY 'Fin fichier'
                   SET FIN-FICHIER TO TRUE
           END-READ
           IF NOT-FIN-FICHIER
              ADD 1 TO NOMBRE-LIGNES
              MOVE 0 TO INDEX-COLONNE
              PERFORM TRAITER-LIGNE
           END-PERFORM.

       TRAITER-LIGNE.
           UNSTRING LIGNE-FICHIER DELIMITED BY " " INTO
               COLONNE(NOMBRE-LIGNES 1)
               COLONNE(NOMBRE-LIGNES 2)
               COLONNE(NOMBRE-LIGNES 3)
               COLONNE(NOMBRE-LIGNES 4)
               COLONNE(NOMBRE-LIGNES 5)
               COLONNE(NOMBRE-LIGNES 6)
               COLONNE(NOMBRE-LIGNES 7)
               COLONNE(NOMBRE-LIGNES 8)
               COLONNE(NOMBRE-LIGNES 9)
               COLONNE(NOMBRE-LIGNES 10)
               END-UNSTRING.

       AFFICHER-TABLEAU.
           PERFORM VARYING INDEX-LIGNE FROM 1 BY 1 
           UNTIL INDEX-LIGNE > NOMBRE-LIGNES
               DISPLAY "Ligne : " LIGNE(INDEX-LIGNE)
           END-PERFORM.   

       AFFICHER-LIGNE.
           PERFORM VARYING INDEX-LIGNE FROM 1 BY 1 
              UNTIL INDEX-LIGNE > NOMBRE-LIGNES
                DISPLAY "Ligne : " LIGNE(INDEX-LIGNE)
                PERFORM VARYING INDEX-COLONNE FROM 1 BY 1 
                  UNTIL INDEX-COLONNE > MAX-COLONNES
                     IF COLONNE(INDEX-LIGNE INDEX-COLONNE)
                      NOT EQUAL TO ZEROES
                         DISPLAY COLONNE(INDEX-LIGNE INDEX-COLONNE) " "
                            WITH NO ADVANCING
                     END-IF
                END-PERFORM
           END-PERFORM.
           
       TRAITER-TABLEAU.
           PERFORM VARYING INDEX-LIGNE FROM 1 BY 1 
              UNTIL INDEX-LIGNE > NOMBRE-LIGNES
                SET RAPPORT-OK TO TRUE
      D         Display '*** Traitement :' ligne(INDEX-LIGNE) '***'
                PERFORM TRAITER-LIGNE-RAPPORT
                IF RAPPORT-OK
      D            display 'Rapport Ok'
                   ADD 1 TO WS-NB-RAPPORTS-OK
                   ADD 1 TO WS-NB-RAPPORTS-OK-DUMP
                else
      D            display 'Rapport Ko'
                   SET RAPPORT-OK-DUMP TO TRUE
                   perform traitement-dumper
                   IF RAPPORT-OK-DUMP
      D               display 'Rapport Ok avec DUMP'
                      ADD 1 TO WS-NB-RAPPORTS-OK-DUMP
      D            else
      D               display 'Rapport Ko malgré DUMP'
                   end-if
                END-IF
           END-PERFORM. 

       TRAITER-LIGNE-RAPPORT.
           COMPUTE WS-NB-PROB = 0
           PERFORM VARYING INDEX-COLONNE FROM 1 BY 1 
              UNTIL INDEX-COLONNE > MAX-COLONNES
               IF COLONNE(INDEX-LIGNE INDEX-COLONNE)
                  NOT EQUAL TO ZEROES
      D           display 'traitement de : ' 
      D            COLONNE(INDEX-LIGNE INDEX-COLONNE)
                
                   if INDEX-COLONNE = 1
                      if COLONNE(INDEX-LIGNE 1) >
                         COLONNE(INDEX-LIGNE 2)
                           SET RAPPORT-ASC TO TRUE
                      else
                           SET RAPPORT-DESC TO TRUE
                      end-if
                   else
                      COMPUTE IND-COL-INF = INDEX-COLONNE - 1
                      if (COLONNE(INDEX-LIGNE IND-COL-INF)
                             > COLONNE(INDEX-LIGNE INDEX-COLONNE))
                          AND RAPPORT-DESC
                             SET RAPPORT-KO TO TRUE
                             ADD 1 TO WS-NB-PROB
      D                      display 'changement de direction'
                      end-if 
                      if (COLONNE(INDEX-LIGNE IND-COL-INF)
                           < COLONNE(INDEX-LIGNE INDEX-COLONNE))
                           AND RAPPORT-ASC
                             SET RAPPORT-KO TO TRUE
                             ADD 1 TO WS-NB-PROB
      D                      display 'changement de direction'
                      end-if
                      COMPUTE ws-diff = FUNCTION ABS(
                        COLONNE(INDEX-LIGNE INDEX-COLONNE) - 
                        COLONNE(INDEX-LIGNE IND-COL-INF))
      D               display "pour " COLONNE(INDEX-LIGNE INDEX-COLONNE)
      D                       " diff :" ws-diff
                      if ws-diff > 3 OR ws-diff = 0
                         ADD 1 TO WS-NB-PROB
                         SET RAPPORT-KO TO TRUE
                      end-if
                  END-IF
               END-IF
           END-PERFORM.

       traitement-dumper.
           SET FIN-TRAIT-DUMP-KO TO TRUE
           perform varying index-a-supp from 1 by 1 
             until index-a-supp > MAX-COLONNES OR FIN-TRAIT-DUMP-OK

              initialize TABLEAU-DUMPER
              move 1 to INDEX-COLONNE-DUMP
              perform varying INDEX-COLONNE FROM 1 by 1
                 UNTIL INDEX-COLONNE > MAX-COLONNES
                  if INDEX-COLONNE not EQUAL index-a-supp
                     MOVE COLONNE(INDEX-LIGNE INDEX-COLONNE)
                        TO COLONNE-DUMP(INDEX-COLONNE-DUMP)
                     ADD 1 To INDEX-COLONNE-DUMP
                  end-if
              END-PERFORM
              SET RAPPORT-OK-DUMP TO TRUE
              perform TRAITER-LIGNE-DUMP
              IF RAPPORT-OK-DUMP
                 SET FIN-TRAIT-DUMP-OK TO TRUE
              END-IF
           end-perform
           IF FIN-TRAIT-DUMP-KO
              SET RAPPORT-KO-DUMP TO TRUE
           END-IF.

       TRAITER-LIGNE-DUMP.
           PERFORM VARYING INDEX-COLONNE FROM 1 BY 1 
              UNTIL INDEX-COLONNE > MAX-COLONNES
               IF COLONNE-DUMP(INDEX-COLONNE)
                  NOT EQUAL TO ZEROES
      D           display 'traitement de : ' 
      D            COLONNE-DUMP(INDEX-COLONNE)
                
                   if INDEX-COLONNE = 1
                      if COLONNE-DUMP(1) >
                         COLONNE-DUMP(2)
                           SET RAPPORT-ASC TO TRUE
                      else
                           SET RAPPORT-DESC TO TRUE
                      end-if
                   else
                      COMPUTE IND-COL-INF = INDEX-COLONNE - 1
                      if (COLONNE-DUMP(IND-COL-INF)
                             > COLONNE-DUMP(INDEX-COLONNE))
                          AND RAPPORT-DESC
                             SET RAPPORT-KO-DUMP TO TRUE
      D                      display 'changement de direction'
                      end-if 
                      if (COLONNE-DUMP(IND-COL-INF)
                           < COLONNE-DUMP(INDEX-COLONNE))
                           AND RAPPORT-ASC
                             SET RAPPORT-KO-DUMP TO TRUE
      D                      display 'changement de direction'
                      end-if
                      COMPUTE ws-diff = FUNCTION ABS(
                        COLONNE-DUMP(INDEX-COLONNE) - 
                        COLONNE-DUMP(IND-COL-INF))
      D               display "pour " COLONNE-DUMP(INDEX-COLONNE)
      D                       " diff :" ws-diff
                      if ws-diff > 3 OR ws-diff = 0
                         SET RAPPORT-KO-DUMP TO TRUE
                      end-if
                  END-IF
               END-IF
           END-PERFORM.