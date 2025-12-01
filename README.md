# proyecto-pdi

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

## Estructura del Código

- `unit1.pas`: Formulario principal con event handlers
- `ImageProcessing.pas`: Algoritmos de procesamiento de imágenes
- `UIHelpers.pas`: Utilidades de interfaz de usuario
- `FormFourier.pas`: Transformada de Fourier y filtros de frecuencia
- `FormHistogram.pas`: Visualización de histogramas
- `FormBinarize.pas`: Binarización adaptativa
- `FormGamma.pas`: Corrección gamma
