# pr## Características

- **Transformaciones**: Escala (bilinear), Rotación 90°
- **Filtros**: Contraste, Gamma, Binarización, HSV
- **Bordes**: Sobel, Prewitt, Diferencia
- **Espaciales**: Suavizado recortado, Textura (LBP), Transformada de Fourier
- **ALU**: Operaciones lógicas (AND, OR, XOR, NOT) y aritméticas (ADD, SUB, MUL, DIV)
- **Análisis**: Histogramas RGB/HSVpdi

Aplicación de procesamiento de imágenes en Lazarus/Free Pascal.

## Características

- **Transformaciones**: Escala (bilineal), Rotación 90°
- **Filtros**: Contraste, Gamma, Binarización, HSV
- **Bordes**: Sobel, Prewitt, Diferencia
- **Espaciales**: Suavizado recortado, Textura (LBP), Transformada de Fourier
- **Análisis**: Histogramas RGB/HSV

## Transformada de Fourier

⚠️ **IMPORTANTE**: La implementación usa DFT directa (no FFT optimizada), lo que resulta en procesamiento muy lento para imágenes grandes.

### Recomendaciones:
- ✅ Usar imágenes pequeñas: 128×128 o 256×256
- ⚠️ Imágenes 512×512 pueden tardar 5-10 minutos
- ❌ Evitar imágenes mayores a 512×512

Ver `FOURIER_NOTAS.md` para más detalles técnicos.

## Compilación

```bash
lazbuild --build-all project1.lpi
```

## Operaciones ALU

El módulo ALU (Arithmetic Logic Unit) permite realizar operaciones entre dos imágenes o entre una imagen y una constante:

### Operaciones Lógicas:
- **AND**: Intersección (máscaras)
- **OR**: Unión (fusión)
- **XOR**: Diferencia exclusiva (detección de cambios)
- **NOT**: Complemento (negativo)

### Operaciones Aritméticas:
- **ADD**: Suma con saturación (aclarar)
- **SUB**: Resta con saturación (oscurecer)
- **MUL**: Multiplicación normalizada (modulación)
- **DIV**: División normalizada (corrección)

Ver `ALU_DOCUMENTACION.md` para detalles completos y ejemplos de uso.

## Estructura del Código

- `unit1.pas`: Formulario principal con event handlers
- `ImageProcessing.pas`: Algoritmos de procesamiento de imágenes
- `UIHelpers.pas`: Utilidades de interfaz de usuario
- `FormFourier.pas`: Transformada de Fourier y filtros de frecuencia
- `FormALU.pas`: Operaciones ALU (lógicas y aritméticas)
- `FormHistogram.pas`: Visualización de histogramas
- `FormBinarize.pas`: Binarización adaptativa
- `FormGamma.pas`: Corrección gamma
