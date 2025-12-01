unit Constants;

{$mode objfpc}{$H+}

interface

const
  // Información de la Aplicación
  NOMBRE_APP = 'Proyecto PDI';
  VERSION_APP = '1.0.0';
  TITULO_APP = 'Proyecto Procesamiento Digital de Imágenes';
  
  // Límites de Imagen
  ANCHO_MAX_IMAGEN = 20000;
  ALTO_MAX_IMAGEN = 20000;
  TAMANO_MAX_ARCHIVO_MB = 100;  // Máximo tamaño de archivo en MB
  
  // Advertencias de Procesamiento
  TAMANO_ADVERTENCIA_FFT = 65536;  // 256x256 píxeles
  TAMANO_RECOMENDADO_FFT = 16384;  // 128x128 píxeles
  
  // Valores por Defecto
  UMBRAL_POR_DEFECTO = 128;
  GAMMA_POR_DEFECTO = 1.0;
  ANGULO_ROTACION_DEFECTO = 90.0;
  FACTOR_ESCALA_DEFECTO = 0.5;
  
  // Límites de Operaciones
  VALOR_MIN_PIXEL = 0;
  VALOR_MAX_PIXEL = 255;
  
  // Modos de Color
  MODO_COLOR_RGB = 1;
  MODO_COLOR_HSV = 3;
  
  // Filtros Fourier
  FILTRO_PASA_BAJAS = 0;
  FILTRO_PASA_ALTAS = 1;
  FILTRO_PASA_BANDA = 2;
  FILTRO_RECHAZA_BANDA = 3;
  
  // Operaciones ALU
  OP_ALU_AND = 0;
  OP_ALU_OR = 1;
  OP_ALU_XOR = 2;
  OP_ALU_NOT = 3;
  OP_ALU_ADD = 4;
  OP_ALU_SUB = 5;
  OP_ALU_MUL = 6;
  OP_ALU_DIV = 7;
  
  // Mensajes de Usuario
  MSG_SIN_IMAGEN = 'Primero debes cargar una imagen';
  MSG_ARCHIVO_MUY_GRANDE = 'El archivo es muy grande (>%d MB). ¿Continuar?';
  MSG_IMAGEN_MUY_GRANDE = 'Imagen demasiado grande. Máximo: %dx%d';
  MSG_ADVERTENCIA_FFT = 'La imagen es grande (%dx%d = %d píxeles). ' +
                        'La Transformada de Fourier puede tardar varios minutos. ' +
                        '¿Deseas continuar?';
  MSG_OPERACION_IRREVERSIBLE = 'Esta operación no se puede deshacer. ¿Continuar?';
  MSG_ERROR_CARGAR_IMAGEN = 'Error al cargar imagen: %s';
  MSG_ERROR_GUARDAR_IMAGEN = 'Error al guardar imagen: %s';
  
  // Configuración UI
  INTERVALO_ACTUALIZACION_PROGRESO = 5;  // Actualizar progreso cada 5%
  ANCHO_MIN_FORMULARIO = 900;
  ALTO_MIN_FORMULARIO = 650;

implementation

end.
