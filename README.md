# Sistema Bancario en COBOL

## Descripcion

Este proyecto es un **sistema bancario basico desarrollado en COBOL**.
Permite simular operaciones bancarias como depositos y retiros, utilizando archivos secuenciales para el almacenamiento de datos.

El objetivo del proyecto es **aprender la estructura y logica del lenguaje COBOL** aplicado a un caso real del sector financiero.

## Funcionalidades

- Crear cuentas bancarias.
- Registrar depositos.
- Registrar retiros.
- Validar montos ingresados.
- Almacenamiento en archivos `.dat`

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