# Sistema Bancario en COBOL

## Descripcion

Este proyecto es un **sistema bancario basico desarrollado en COBOL**.
Permite simular operaciones bancarias como depositos y retiros, utilizando archivos secuenciales para el almacenamiento de datos.

El objetivo del proyecto es **aprender la estructura y logica del lenguaje COBOL** aplicado a un caso real del sector financiero.

## Funcionalidades

- Crear cuentas bancarias `Create-Account`.
- Registrar depositos `Deposit-Money`.
- Registrar retiros `Withdraw-Money`.
- Consultar saldo actual `Check-Balance`.
- Listar las cuentas registradas `List-Account`.
- Limpiar el archivo `.dat` con `Clear-Account-File`(Funcionalidad en evaluacion). 
- Almacenamiento en archivos `.dat` lectura y escritura.

## Fujo del programa 
- `MAIN-LOGIN`
    - Muestra el menu del program.
    - Se encarga de controlar el ciclo de ejecucion
- `DISPLAY-MENU`
    - Se listan las opciones.
- `PROCESS-OPTION`
    - Evalua la opcion ingresada
    - Redirige a la rutina correspondiente
- `CREATE-ACCOUNT`
    - Se ingresa el Id de la cuenta -> Evaluamos si existe la cuenta `Validate-ID-In-File`
    - Una vez validado Id es guardado y el nombre del titular.
- `DEPOSIT-MONEY`
    - HACE EL LLAMADO A -> `FIND-ACCOUNT` para buscar la cuenta.
    - Si existe -> Ingresamos monto del deposito.
    - No Existe -> Solicita de nuevo el Id de la cuenta.
- `WITHDRAW-MONEY`
    - HACE EL LLAMADO A -> `FIND-ACCOUNT` para buscar la cuenta.
    - Si existe -> Ingresamo el monto a retirar.
    - No existe -> Solicita nuevamente el Id de la cuenta.
- `CHECK-BALANCE`
    - HACE EL LLAMADO A -> `FIND-ACCOUNT` para buscar la cuenta.
    - Si existe -> Muestra el estado de la cuenta
    - No existe -> Solicita nuevamente el Id de la cuenta.

## Tecnologias 

- Lenguaje: **COBOL**
- Compilador: **GnuCOBOL (cobc)**
- Editor: **Visual Studio Code**
- Sistema de archivos: **Secuencial**

## Ejecucion

1. Compilar el programa:
```bash
cobc -x sistema-bancario.cbl
./sistema-bancario
```
---
### Estructura del proyecto
- `sistema-bancario.cbl` -> Programa principal
- `cuentas.dat` -> Archivo de cuentas
- `transacciones.dat` -> Archivos de movimientos
- `README.md` -> Documentacion del proyecto

## 👤Autor
- **Jose Roberto Vilca Terrazas**
- Proyecto academico / personal
- Ano: 2025

## 📚 Referencia
- Tutorial y material base sobre COBOL bancario:
https://www.youtube.com/watch?v=J7wsIAYvb-I&t=1302s