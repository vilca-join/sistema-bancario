      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. sistema-bancario.
      *Aca se Describe el entorno: Archivos, Dispositivos, etc.
       ENVIRONMENT DIVISION.
      ******************************************************************
      *Aqui declare "AccountFile" es un archivo secuencial.
      *Palabra reservada OPTIONAL para que no reviente el programa.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL AccountFile ASSIGN TO "cuentas.dat"
               ORGANISATION IS SEQUENTIAL.
           SELECT OPTIONAL TransactionFile ASSIGN TO "transacciones.dat"
               ORGANIZATION IS SEQUENTIAL.
      ******************************************************************
      *Aqui declaramos toda la estructura de datos del programa.
       DATA DIVISION.
      *FILE SECTION Estructura de los registros de archivo
       FILE SECTION.
      *FD (FILE DESCRIPTION) Describe el formato del registro asociado
      *AccountRecord: es el registro que se lee
      *PIC Definimos el tipo y la longitud
       FD  AccountFile.
       01  AccountRecord.
           05  Account-ID       PIC 9(5).
           05  Account-Holder   PIC X(30).
           05  Account-Balance  PIC 9(7)V99.
      *Aqui se declaro un campo de un caracter y este tiene niveles
      *Los niveles 88 son condiciones de nivel 88(condition names):
      *Deposit Value = 'D'
      *Withdraw Values = 'W'
       FD  TransactionFile.
       01  TransactionRecord.
           05  Trans-Account-ID    PIC 9(5).
           05  Trans-Type          PIC X(1).
               88 Deposit          VALUE 'D'.
               88 Withdraw         VALUE 'W'.
           05  Trans-Amount        PIC 9(7)V99.
      *Aqui encontramos las variables que se usan durante la ejecucion.
       WORKING-STORAGE SECTION.
       01  Prompt-Account-ID       PIC X(25)
       VALUE "Ingrese ID de la cuenta: ".
       01  Prompt-Holder-Name      PIC X(28)
       VALUE "Ingrese nombre del titular: ".
       01  Prompt-Amount           PIC X(25)
       VALUE "Ingrese monto: ".
       01  Invalid-Amount          PIC X(35)
       VALUE "Monto no valido, intente de nuevo.".
       01  Insufficient-Funds      PIC X(51)
       VALUE "Fondos insuficientes para realizar la transaccion.".


       01  User-Option             PIC X.
       01  Found-Account           PIC X VALUE 'N'.
       01  Account-Search-ID       PIC 9(5).
       01  Transaction-Amount      PIC 9(7)V99.

       01  Eof-Accounts            PIC X VALUE 'N'.
       01  Confirm-Delete          PIC X VALUE 'N'.
       01  Ws-Saldo-Edit           PIC Z(7)9.99.
       
       01  Ws-Search-ID            PIC 9(5).
       01  Ws-Found                PIC X VALUE 'N'.
           88 Id-Exists            VALUE 'Y'.
           88 Id-Does-Exist        VALUE 'N'.
       01  Ws-EOF                  PIC X VALUES 'N'.
           88 End-File             VALUE 'Y'.
           88 No-End               VALUE 'N'.
           

      *Aqui empezamos a desarrollar la logica del programa.
       PROCEDURE DIVISION.
       Main-Login.
           PERFORM Display-Menu
           PERFORM UNTIL User-Option = '7  '
           PERFORM Process-Option
           PERFORM Display-Menu
           END-PERFORM.
           STOP RUN.

       Validate-ID-In-File.           
           MOVE 'N' TO Ws-Found
           MOVE 'N' TO Ws-EOF
           
           OPEN INPUT AccountFile
           
           PERFORM UNTIL End-File OR Id-Exists
               READ AccountFile
                   AT END
                       MOVE 'Y' TO Ws-EOF
                   NOT AT END
                       IF Account-ID = Ws-Search-ID
                           MOVE 'Y' TO Ws-Found
                       END-IF
               END-READ
           END-PERFORM
           CLOSE AccountFile
           IF Id-Exists
               DISPLAY "El Id existe: ", Ws-Search-ID
           ELSE
               DISPLAY "El Id no existe: ", Ws-Search-ID
           END-IF.
           
       
       Display-Menu.
           DISPLAY "========SISTEMA BANCARIO========"
           DISPLAY "1. Crear Cuenta"
           DISPLAY "2. Depositar dinero"
           DISPLAY "3. Retirar dinero"
           DISPLAY "4. Consultar saldo"
           DISPLAY "5. Listar cuentas"
           DISPLAY "6. Limpiar cuentas"
           DISPLAY "7. Salir"
           DISPLAY "Seleccione una opcion: "
           ACCEPT User-Option.
       Process-Option.
           EVALUATE User-Option
               WHEN '1'
                   PERFORM Create-Account
               WHEN '2'
                   PERFORM Deposit-Money
               WHEN '3'
                   PERFORM Withdraw-Money
               WHEN '4'
                   PERFORM Check-Balance
               WHEN '5'
                   PERFORM List-Account
               WHEN '6'
                   PERFORM Clear-Account-File
               WHEN OTHER
                   DISPLAY "Opcion no valida, intente de nuevo."
           END-EVALUATE.

       Create-Account.                    
           DISPLAY Prompt-Account-ID
           ACCEPT Ws-Search-ID
           
           PERFORM Validate-ID-In-File
           
           IF Id-Exists 
               DISPLAY "Ese Id ya existe. Intente con otro."
               EXIT PARAGRAPH
           END-IF
           
           OPEN EXTEND AccountFile
           MOVE Ws-Search-ID TO Account-ID
           DISPLAY Prompt-Holder-Name
           ACCEPT Account-Holder
           MOVE 0 TO Account-Balance
           WRITE AccountRecord
           DISPLAY "Cuenta creada exitosamente.", Account-ID.
           CLOSE AccountFile.

       Deposit-Money.
           OPEN I-O AccountFile
           PERFORM Find-Account
           IF Found-Account = 'Y'
               DISPLAY Prompt-Amount
               ACCEPT Transaction-Amount
           IF Transaction-Amount > 0
               ADD Transaction-Amount TO Account-Balance
               REWRITE AccountRecord
               MOVE 'D' TO Trans-Type
               PERFORM Record-Transaction
               DISPLAY "Deposito exitoso."
           ELSE
               DISPLAY Invalid-Amount
           END-IF
           ELSE
               DISPLAY "Cuenta no encontrada."
           END-IF.
           CLOSE AccountFile.

       Withdraw-Money.
           OPEN I-O AccountFile
           PERFORM Find-Account
           IF Found-Account = 'Y'
               DISPLAY Prompt-Amount
               ACCEPT Transaction-Amount
               IF Transaction-Amount > 0
               AND Transaction-Amount <= Account-Balance
                   SUBTRACT Transaction-Amount FROM Account-Balance
                   MOVE 'W' TO Trans-Type
                   PERFORM Record-Transaction
                   REWRITE AccountRecord
                   DISPLAY "Retiro exitoso."
               ELSE IF Transaction-Amount > Account-Balance
                   DISPLAY Insufficient-Funds
               ELSE
                   DISPLAY Invalid-Amount
               END-IF
           ELSE
               DISPLAY "Cuenta no encontrada."
           END-IF.
           CLOSE AccountFile.

       Check-Balance.
           OPEN I-O AccountFile
           PERFORM Find-Account
           IF Found-Account = 'Y'
               MOVE Account-Balance TO Ws-Saldo-Edit
               DISPLAY "Saldo actual de la cuenta: ", Ws-Saldo-Edit
           ELSE
               DISPLAY "Cuenta no encontrada."
           END-IF.
           CLOSE AccountFile.
       Find-Account.
           MOVE 'N' TO Found-Account
           DISPLAY Prompt-Account-ID
           ACCEPT Account-Search-ID
           PERFORM UNTIL Found-Account = 'Y'
           READ AccountFile
            AT END
               DISPLAY "Cuenta no encontrada."
               EXIT PERFORM
            NOT AT END
               IF Account-ID = Account-Search-ID
                   MOVE 'Y' TO Found-Account
               END-IF
               END-READ
           END-PERFORM.

       List-Account.
           OPEN INPUT AccountFile
           MOVE 'N' TO Eof-Accounts

           READ AccountFile
               AT END
                   DISPLAY "Actualmente no tienes cuentas creadas"
                   CLOSE AccountFile
                   EXIT PARAGRAPH
           END-READ

           PERFORM UNTIL Eof-Accounts = 'Y'
           MOVE Account-Balance TO Ws-Saldo-Edit
           DISPLAY "-----------------------"
           DISPLAY "ID: ", Account-ID
           DISPLAY "Titular: ", Account-Holder
           DISPLAY "Saldo: ", Ws-Saldo-Edit
               READ AccountFile
                   AT END
                       MOVE 'Y' TO Eof-Accounts
               END-READ
           END-PERFORM
           CLOSE AccountFile.

       Clear-Account-File.
           DISPLAY "Estas seguro de eliminar estos registros?"
           DISPLAY "Responde SI / NO "
           ACCEPT Confirm-Delete.

           IF Confirm-Delete = 'Y' OR Confirm-Delete = 'y'
               OPEN OUTPUT AccountFile
               CLOSE AccountFile
               DISPLAY "Archivos cuenta.dat limpio."
           ELSE
               DISPLAY "Operacion cancelada."
           END-IF.

       Record-Transaction.
           OPEN EXTEND TransactionFile
           MOVE Account-ID TO Trans-Account-ID
           MOVE Transaction-Amount TO Trans-Amount
           WRITE TransactionRecord
           CLOSE TransactionFile.
