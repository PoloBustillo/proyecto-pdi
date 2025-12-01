unit ImageProcessing;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Math;

type
  RGB_MATRIX = array of array of array of byte;
  HSV_MATRIX = array of array of array of Double;
  GRAY_SCALE_MATRIX = array of array of Byte;
  BYTE_LUT = array[0..255] of Byte;
  
  THistogramData = record
    Red: array[0..255] of Integer;
    Green: array[0..255] of Integer;
    Blue: array[0..255] of Integer;
    Intensity: array[0..255] of Integer;
  end;

{ Funciones de conversión de imágenes }
procedure CopyImageToMatrix(imageHeight, imageWidth: Integer; B: TBitmap;
  var matrix: RGB_MATRIX);
procedure CopyMatrixToImage(imageHeight, imageWidth: Integer; 
  const matrix: RGB_MATRIX; var B: TBitmap);

{ Funciones de conversión de color }
procedure RGBToHSV(r, g, b: Byte; out H, S, V: Double);
procedure HSVToRGB(H, S, V: Double; out r, g, b: Byte);
procedure RGBMatrixToHSVMatrix(imageHeight, imageWidth: Integer;
  const RGB: RGB_MATRIX; var HSV: HSV_MATRIX);
procedure HSVMatrixToRGBMatrix(imageHeight, imageWidth: Integer;
  const HSV: HSV_MATRIX; var RGB: RGB_MATRIX);

{ Funciones de procesamiento }
procedure BinarizeMatrix(const src: RGB_MATRIX; var dst: RGB_MATRIX;
  imageHeight, imageWidth: Integer; modeIndex: Integer; threshold: Byte);
procedure MediumRangeGrayScale(imageHeight, imageWidth: Integer;
  const matrix: RGB_MATRIX; var grayMatrix: RGB_MATRIX);
procedure HSVToGrayScale(imageHeight, imageWidth: Integer;
  var hsvMatrix: HSV_MATRIX);
procedure ApplyGammaCorrection(var matrix: RGB_MATRIX; imageHeight, 
  imageWidth: Integer; gamma: Double);
procedure ApplyGammaToHSVValue(imageHeight, imageWidth: Integer;
  const rgbResult: RGB_MATRIX; var hsvMatrix: HSV_MATRIX);
procedure ApplyContrast(var matrix: RGB_MATRIX; imageHeight, imageWidth: Integer;
  methodIndex: Integer; percentile: Double);
procedure AutoContrast(imageHeight, imageWidth: Integer; var matrix: RGB_MATRIX);
procedure AutoContrastHSV(imageHeight, imageWidth: Integer; var hsvMatrix: HSV_MATRIX);

{ Filtros de detección de bordes }
procedure EdgeDetectionDifference(imageHeight, imageWidth: Integer; 
  const srcMatrix: RGB_MATRIX; var dstMatrix: RGB_MATRIX);
procedure EdgeDetectionPrewitt(imageHeight, imageWidth: Integer; 
  const srcMatrix: RGB_MATRIX; var dstMatrix: RGB_MATRIX);
procedure EdgeDetectionSobel(imageHeight, imageWidth: Integer; 
  const srcMatrix: RGB_MATRIX; var dstMatrix: RGB_MATRIX);

{ Filtros de suavizado y textura }
procedure TrimmedSmoothing(imageHeight, imageWidth: Integer;
  const srcMatrix: RGB_MATRIX; var dstMatrix: RGB_MATRIX;
  maskSize: Integer; trimAmount: Integer);
procedure EncodedTexture(imageHeight, imageWidth: Integer;
  const srcMatrix: RGB_MATRIX; var dstMatrix: RGB_MATRIX);

{ Transformaciones geométricas }
procedure ScaleUpBilinear(imageHeight, imageWidth: Integer;
  const srcMatrix: RGB_MATRIX; var dstMatrix: RGB_MATRIX;
  out newWidth, newHeight: Integer);
procedure ScaleDownBilinear(imageHeight, imageWidth: Integer;
  const srcMatrix: RGB_MATRIX; var dstMatrix: RGB_MATRIX;
  out newWidth, newHeight: Integer);
procedure RotateRight90(imageHeight, imageWidth: Integer;
  const srcMatrix: RGB_MATRIX; var dstMatrix: RGB_MATRIX;
  out newWidth, newHeight: Integer);
procedure RotateLeft90(imageHeight, imageWidth: Integer;
  const srcMatrix: RGB_MATRIX; var dstMatrix: RGB_MATRIX;
  out newWidth, newHeight: Integer);

{ Funciones de análisis }
procedure GenerateHistogram(imageHeight, imageWidth: Integer; 
  const matrix: RGB_MATRIX; out histData: THistogramData);
procedure BuildGammaLUT(gamma: Double; var lut: BYTE_LUT);
function PercentileFromHist(const hist: array of Integer; total: Integer; 
  p: Double): Integer;

implementation

{ Implementación de funciones de conversión }

procedure CopyImageToMatrix(imageHeight, imageWidth: Integer; B: TBitmap;
  var matrix: RGB_MATRIX);
var
  i, j, k: Integer;
  P: PByte;
begin
  if (imageWidth = 0) or (imageHeight = 0) then
    Exit;

  SetLength(matrix, imageWidth, imageHeight, 3);
  B.BeginUpdate;
  try
    for i := 0 to imageHeight - 1 do
    begin
      P := B.ScanLine[i];
      for j := 0 to imageWidth - 1 do
      begin
        k := 3 * j;
        matrix[j, i, 0] := P[k + 2];  // R
        matrix[j, i, 1] := P[k + 1];  // G
        matrix[j, i, 2] := P[k + 0];  // B
      end;
    end;
  finally
    B.EndUpdate;
  end;
end;

procedure CopyMatrixToImage(imageHeight, imageWidth: Integer; 
  const matrix: RGB_MATRIX; var B: TBitmap);
var
  i, j, k: Integer;
  P: PByte;
begin
  if (imageWidth = 0) or (imageHeight = 0) then
    Exit;

  B.BeginUpdate;
  try
    for i := 0 to imageHeight - 1 do
    begin
      P := B.ScanLine[i];
      for j := 0 to imageWidth - 1 do
      begin
        k := 3 * j;
        P[k + 2] := matrix[j, i, 0];  // R
        P[k + 1] := matrix[j, i, 1];  // G
        P[k + 0] := matrix[j, i, 2];  // B
      end;
    end;
  finally
    B.EndUpdate;
  end;
end;

procedure RGBToHSV(r, g, b: Byte; out H, S, V: Double);
var
  rf, gf, bf, cmax, cmin, delta: Double;
begin
  rf := r / 255.0;
  gf := g / 255.0;
  bf := b / 255.0;

  cmax := Max(rf, Max(gf, bf));
  cmin := Min(rf, Min(gf, bf));
  delta := cmax - cmin;

  // V: 0.0 - 1.0
  V := cmax;
  
  // S: 0.0 - 1.0
  if cmax = 0 then
    S := 0
  else
    S := delta / cmax;

  // H: 0.0 - 360.0 grados
  if delta = 0 then
    H := 0
  else
  begin
    if cmax = rf then
      H := (gf - bf) / delta
    else if cmax = gf then
      H := 2.0 + (bf - rf) / delta
    else
      H := 4.0 + (rf - gf) / delta;

    H := H * 60.0;
    if H < 0 then
      H := H + 360.0;
  end;
end;

procedure HSVToRGB(H, S, V: Double; out r, g, b: Byte);
var
  C, X, m: Double;
  Hp: Double;
  Rp, Gp, Bp: Double;
  rVal, gVal, bVal: Double;
begin
  // H: 0.0 - 360.0 grados
  // S: 0.0 - 1.0
  // V: 0.0 - 1.0
  
  C := V * S;
  Hp := H / 60.0;
  // Fórmula correcta: X = C × (1 - |(Hp mod 2) - 1|)
  X := C * (1.0 - Abs((Hp - Int(Hp / 2.0) * 2.0) - 1.0));
  
  if (Hp >= 0) and (Hp < 1) then
  begin
    Rp := C; Gp := X; Bp := 0;
  end
  else if (Hp >= 1) and (Hp < 2) then
  begin
    Rp := X; Gp := C; Bp := 0;
  end
  else if (Hp >= 2) and (Hp < 3) then
  begin
    Rp := 0; Gp := C; Bp := X;
  end
  else if (Hp >= 3) and (Hp < 4) then
  begin
    Rp := 0; Gp := X; Bp := C;
  end
  else if (Hp >= 4) and (Hp < 5) then
  begin
    Rp := X; Gp := 0; Bp := C;
  end
  else
  begin
    Rp := C; Gp := 0; Bp := X;
  end;
  
  m := V - C;
  rVal := (Rp + m) * 255.0;
  gVal := (Gp + m) * 255.0;
  bVal := (Bp + m) * 255.0;
  
  // Clamp y convertir a byte
  if rVal < 0 then rVal := 0 else if rVal > 255 then rVal := 255;
  if gVal < 0 then gVal := 0 else if gVal > 255 then gVal := 255;
  if bVal < 0 then bVal := 0 else if bVal > 255 then bVal := 255;
  
  r := Round(rVal);
  g := Round(gVal);
  b := Round(bVal);
end;

procedure RGBMatrixToHSVMatrix(imageHeight, imageWidth: Integer;
  const RGB: RGB_MATRIX; var HSV: HSV_MATRIX);
var
  i, j: Integer;
  r, g, b: Byte;
  h, s, v: Double;
begin
  SetLength(HSV, imageWidth, imageHeight, 3);
  for i := 0 to imageHeight - 1 do
    for j := 0 to imageWidth - 1 do
    begin
      r := RGB[j, i, 0];
      g := RGB[j, i, 1];
      b := RGB[j, i, 2];
      RGBToHSV(r, g, b, h, s, v);
      HSV[j, i, 0] := h;  // H: 0.0 - 360.0
      HSV[j, i, 1] := s;  // S: 0.0 - 1.0
      HSV[j, i, 2] := v;  // V: 0.0 - 1.0
    end;
end;

procedure HSVMatrixToRGBMatrix(imageHeight, imageWidth: Integer;
  const HSV: HSV_MATRIX; var RGB: RGB_MATRIX);
var
  i, j: Integer;
  r, g, b: Byte;
  h, s, v: Double;
begin
  SetLength(RGB, imageWidth, imageHeight, 3);
  for i := 0 to imageHeight - 1 do
    for j := 0 to imageWidth - 1 do
    begin
      h := HSV[j, i, 0];  // H: 0.0 - 360.0
      s := HSV[j, i, 1];  // S: 0.0 - 1.0
      v := HSV[j, i, 2];  // V: 0.0 - 1.0
      HSVToRGB(h, s, v, r, g, b);
      RGB[j, i, 0] := r;
      RGB[j, i, 1] := g;
      RGB[j, i, 2] := b;
    end;
end;

{ Funciones de procesamiento }

procedure BinarizeMatrix(const src: RGB_MATRIX; var dst: RGB_MATRIX;
  imageHeight, imageWidth: Integer; modeIndex: Integer; threshold: Byte);
var
  x, y: Integer;
  r, g, b: Byte;
  val: Integer;
begin
  if (imageWidth = 0) or (imageHeight = 0) then
    Exit;
    
  SetLength(dst, imageWidth, imageHeight, 3);
  for x := 0 to imageWidth - 1 do
    for y := 0 to imageHeight - 1 do
    begin
      r := src[x, y, 0];
      g := src[x, y, 1];
      b := src[x, y, 2];
      
      case modeIndex of
        0: val := (Integer(r) + Integer(g) + Integer(b)) div 3;
        1: val := Round(0.299 * r + 0.587 * g + 0.114 * b);
        2: val := r;
        3: val := g;
        4: val := b;
      else
        val := (Integer(r) + Integer(g) + Integer(b)) div 3;
      end;

      if val >= threshold then
      begin
        dst[x, y, 0] := 255;
        dst[x, y, 1] := 255;
        dst[x, y, 2] := 255;
      end
      else
      begin
        dst[x, y, 0] := 0;
        dst[x, y, 1] := 0;
        dst[x, y, 2] := 0;
      end;
    end;
end;

procedure MediumRangeGrayScale(imageHeight, imageWidth: Integer;
  const matrix: RGB_MATRIX; var grayMatrix: RGB_MATRIX);
var
  i, j: Integer;
  red, green, blue, gray: Byte;
  minimumValue, maximumValue: Byte;
begin
  if (imageWidth = 0) or (imageHeight = 0) then
    Exit;
    
  SetLength(grayMatrix, imageWidth, imageHeight, 3);
  for i := 0 to imageWidth - 1 do
    for j := 0 to imageHeight - 1 do
    begin
      red := matrix[i, j, 0];
      green := matrix[i, j, 1];
      blue := matrix[i, j, 2];
      maximumValue := Max(red, Max(green, blue));
      minimumValue := Min(red, Min(green, blue));
      gray := (maximumValue + minimumValue) div 2;
      grayMatrix[i, j, 0] := gray;
      grayMatrix[i, j, 1] := gray;
      grayMatrix[i, j, 2] := gray;
    end;
end;

procedure HSVToGrayScale(imageHeight, imageWidth: Integer;
  var hsvMatrix: HSV_MATRIX);
var
  x, y: Integer;
begin
  if (imageWidth = 0) or (imageHeight = 0) then
    Exit;
    
  // En HSV, convertir a escala de grises estableciendo S=0
  // Esto elimina todo el color, manteniendo solo el brillo (V)
  for x := 0 to imageWidth - 1 do
    for y := 0 to imageHeight - 1 do
    begin
      hsvMatrix[x, y, 0] := 0;   // H (Matiz) = 0
      hsvMatrix[x, y, 1] := 0;   // S (Saturación) = 0 (sin color -> escala de grises)
      // hsvMatrix[x, y, 2] permanece sin cambios (mantener brillo V original)
    end;
end;

procedure BuildGammaLUT(gamma: Double; var lut: BYTE_LUT);
var
  i: Integer;
  v: Double;
begin
  if gamma <= 0 then
    gamma := 1.0;
  for i := 0 to 255 do
  begin
    if i = 0 then
      lut[i] := 0
    else
    begin
      v := Power((i / 255.0), gamma);
      v := v * 255.0;
      if v < 0.0 then v := 0.0;
      if v > 255.0 then v := 255.0;
      lut[i] := Byte(Round(v));
    end;
  end;
end;

procedure ApplyGammaCorrection(var matrix: RGB_MATRIX; imageHeight, 
  imageWidth: Integer; gamma: Double);
var
  x, y, k: Integer;
  lut: BYTE_LUT;
begin
  if (imageWidth = 0) or (imageHeight = 0) then
    Exit;
    
  BuildGammaLUT(gamma, lut);
  for x := 0 to imageWidth - 1 do
    for y := 0 to imageHeight - 1 do
      for k := 0 to 2 do
        matrix[x, y, k] := lut[matrix[x, y, k]];
end;

procedure ApplyGammaToHSVValue(imageHeight, imageWidth: Integer;
  const rgbResult: RGB_MATRIX; var hsvMatrix: HSV_MATRIX);
var
  tempHSV: HSV_MATRIX;
  x, y: Integer;
begin
  if (imageWidth = 0) or (imageHeight = 0) then
    Exit;
    
  // Convertir el resultado RGB (con gamma aplicado) a HSV temporal
  RGBMatrixToHSVMatrix(imageHeight, imageWidth, rgbResult, tempHSV);
  
  // Copiar solo el canal V del resultado, mantener H y S originales
  for x := 0 to imageWidth - 1 do
    for y := 0 to imageHeight - 1 do
      hsvMatrix[x, y, 2] := tempHSV[x, y, 2]; // Solo actualizar V (canal de brillo)
end;

function PercentileFromHist(const hist: array of Integer; total: Integer; 
  p: Double): Integer;
var
  i: Integer;
  cum: Int64;
  target: Int64;
begin
  if total <= 0 then Exit(0);
  if p <= 0 then Exit(0);
  if p >= 1 then Exit(255);
  
  target := Round(p * total);
  cum := 0;
  for i := 0 to High(hist) do
  begin
    cum := cum + hist[i];
    if cum >= target then
      Exit(i);
  end;
  Result := High(hist);
end;

procedure ApplyContrast(var matrix: RGB_MATRIX; imageHeight, imageWidth: Integer;
  methodIndex: Integer; percentile: Double);
var
  hist: array[0..255] of Integer;
  total, lowVal, highVal: Integer;
  i, x, y, k: Integer;
  lut: array[0..255] of Byte;
  HSVtmp: HSV_MATRIX;
  r, g, b: Byte;
begin
  if (imageWidth = 0) or (imageHeight = 0) then
    Exit;

  if methodIndex in [0, 1] then
  begin
    // Aplicar en luminancia (HSV)
    RGBMatrixToHSVMatrix(imageHeight, imageWidth, matrix, HSVtmp);
    
    for i := 0 to 255 do
      hist[i] := 0;
    for x := 0 to imageWidth - 1 do
      for y := 0 to imageHeight - 1 do
      begin
        // V está en rango 0.0-1.0, convertir a 0-255 para el histograma
        Inc(hist[Round(HSVtmp[x, y, 2] * 255.0)]);
      end;
        
    total := imageWidth * imageHeight;
    
    if methodIndex = 0 then
    begin
      lowVal := 255;
      highVal := 0;
      for i := 0 to 255 do
        if hist[i] > 0 then
        begin
          if i < lowVal then lowVal := i;
          if i > highVal then highVal := i;
        end;
    end
    else
    begin
      lowVal := PercentileFromHist(hist, total, percentile);
      highVal := PercentileFromHist(hist, total, 1.0 - percentile);
    end;

    if highVal <= lowVal then
      for i := 0 to 255 do
        lut[i] := i
    else
      for i := 0 to 255 do
        if i <= lowVal then
          lut[i] := 0
        else if i >= highVal then
          lut[i] := 255
        else
          lut[i] := Byte(Round((i - lowVal) * 255 / (highVal - lowVal)));

    for x := 0 to imageWidth - 1 do
      for y := 0 to imageHeight - 1 do
      begin
        // Aplicar LUT al canal V (convertir de 0.0-1.0 a 0-255, aplicar LUT, volver a 0.0-1.0)
        HSVtmp[x, y, 2] := lut[Round(HSVtmp[x, y, 2] * 255.0)] / 255.0;
        HSVToRGB(HSVtmp[x, y, 0], HSVtmp[x, y, 1], HSVtmp[x, y, 2], r, g, b);
        matrix[x, y, 0] := r;
        matrix[x, y, 1] := g;
        matrix[x, y, 2] := b;
      end;
  end;
end;

procedure GenerateHistogram(imageHeight, imageWidth: Integer; 
  const matrix: RGB_MATRIX; out histData: THistogramData);
var
  x, y, i: Integer;
  r, g, b, intensity: Byte;
begin
  // Inicializar histogramas
  for i := 0 to 255 do
  begin
    histData.Red[i] := 0;
    histData.Green[i] := 0;
    histData.Blue[i] := 0;
    histData.Intensity[i] := 0;
  end;

  // Calcular frecuencias
  for x := 0 to imageWidth - 1 do
    for y := 0 to imageHeight - 1 do
    begin
      r := matrix[x, y, 0];
      g := matrix[x, y, 1];
      b := matrix[x, y, 2];
      Inc(histData.Red[r]);
      Inc(histData.Green[g]);
      Inc(histData.Blue[b]);
      intensity := (r + g + b) div 3;
      Inc(histData.Intensity[intensity]);
    end;
end;

{ Contraste Automático para RGB }
procedure AutoContrast(imageHeight, imageWidth: Integer; var matrix: RGB_MATRIX);
var
  x, y, c: Integer;
  minVal, maxVal: array[0..2] of Byte;  // Min y Max por canal (R, G, B)
  range: array[0..2] of Integer;
  value: Byte;
begin
  // Inicializar valores extremos
  for c := 0 to 2 do
  begin
    minVal[c] := 255;
    maxVal[c] := 0;
  end;
  
  // Encontrar valores mínimos y máximos por canal
  for x := 0 to imageWidth - 1 do
    for y := 0 to imageHeight - 1 do
      for c := 0 to 2 do
      begin
        value := matrix[x, y, c];
        if value < minVal[c] then
          minVal[c] := value;
        if value > maxVal[c] then
          maxVal[c] := value;
      end;
  
  // Calcular rangos por canal
  for c := 0 to 2 do
    range[c] := maxVal[c] - minVal[c];
  
  // Aplicar estiramiento del histograma por canal
  for x := 0 to imageWidth - 1 do
    for y := 0 to imageHeight - 1 do
      for c := 0 to 2 do
      begin
        if range[c] > 0 then
        begin
          // Fórmula: newValue = ((value - min) * 255) / range
          value := matrix[x, y, c];
          matrix[x, y, c] := Round(((value - minVal[c]) * 255.0) / range[c]);
        end;
        // Si range = 0 (todo el canal es uniforme), no modificar
      end;
end;

{ Contraste Automático para HSV (solo canal V) }
procedure AutoContrastHSV(imageHeight, imageWidth: Integer; var hsvMatrix: HSV_MATRIX);
var
  x, y: Integer;
  minV, maxV: Double;
  rangeV: Double;
  v: Double;
begin
  // Inicializar valores extremos para canal V (brillo)
  minV := 1.0;
  maxV := 0.0;
  
  // Encontrar valores mínimo y máximo del canal V
  for x := 0 to imageWidth - 1 do
    for y := 0 to imageHeight - 1 do
    begin
      v := hsvMatrix[x, y, 2];  // Canal V (brillo)
      if v < minV then
        minV := v;
      if v > maxV then
        maxV := v;
    end;
  
  // Calcular rango
  rangeV := maxV - minV;
  
  // Aplicar estiramiento del histograma solo al canal V
  if rangeV > 0.001 then  // Evitar división por cero (umbral para Double)
  begin
    for x := 0 to imageWidth - 1 do
      for y := 0 to imageHeight - 1 do
      begin
        v := hsvMatrix[x, y, 2];
        // Estirar V al rango completo [0.0, 1.0]
        hsvMatrix[x, y, 2] := (v - minV) / rangeV;
        // H y S se preservan sin cambios
      end;
  end;
  // Si rangeV ≈ 0 (toda la imagen tiene el mismo brillo), no modificar
end;

{ Filtros de Detección de Bordes }

{ Detección de bordes por Diferencia (Roberts) }
procedure EdgeDetectionDifference(imageHeight, imageWidth: Integer; 
  const srcMatrix: RGB_MATRIX; var dstMatrix: RGB_MATRIX);
var
  x, y: Integer;
  gx, gy, magnitude: Integer;
  gray: array of array of Byte;
  val: Byte;
begin
  if (imageWidth <= 1) or (imageHeight <= 1) then
    Exit;
    
  SetLength(dstMatrix, imageWidth, imageHeight, 3);
  SetLength(gray, imageWidth, imageHeight);
  
  // Convertir a escala de grises usando luminancia
  for x := 0 to imageWidth - 1 do
    for y := 0 to imageHeight - 1 do
      gray[x, y] := Round(0.299 * srcMatrix[x, y, 0] + 
                          0.587 * srcMatrix[x, y, 1] + 
                          0.114 * srcMatrix[x, y, 2]);
  
  // Aplicar operador de diferencia (Roberts Cross)
  // Gx = |f(x+1,y+1) - f(x,y)|
  // Gy = |f(x+1,y) - f(x,y+1)|
  for x := 0 to imageWidth - 2 do
    for y := 0 to imageHeight - 2 do
    begin
      gx := Abs(gray[x + 1, y + 1] - gray[x, y]);
      gy := Abs(gray[x + 1, y] - gray[x, y + 1]);
      magnitude := gx + gy;
      
      if magnitude > 255 then
        val := 255
      else
        val := Byte(magnitude);
        
      dstMatrix[x, y, 0] := val;
      dstMatrix[x, y, 1] := val;
      dstMatrix[x, y, 2] := val;
    end;
  
  // Rellenar bordes con negro
  for x := 0 to imageWidth - 1 do
  begin
    dstMatrix[x, imageHeight - 1, 0] := 0;
    dstMatrix[x, imageHeight - 1, 1] := 0;
    dstMatrix[x, imageHeight - 1, 2] := 0;
  end;
  for y := 0 to imageHeight - 1 do
  begin
    dstMatrix[imageWidth - 1, y, 0] := 0;
    dstMatrix[imageWidth - 1, y, 1] := 0;
    dstMatrix[imageWidth - 1, y, 2] := 0;
  end;
end;

{ Detección de bordes por Prewitt }
procedure EdgeDetectionPrewitt(imageHeight, imageWidth: Integer; 
  const srcMatrix: RGB_MATRIX; var dstMatrix: RGB_MATRIX);
var
  x, y, i, j: Integer;
  gx, gy, magnitude: Integer;
  gray: array of array of Byte;
  val: Byte;
  // Máscaras de Prewitt
  kernelX: array[-1..1, -1..1] of Integer = (
    (-1, 0, 1),
    (-1, 0, 1),
    (-1, 0, 1)
  );
  kernelY: array[-1..1, -1..1] of Integer = (
    (-1, -1, -1),
    ( 0,  0,  0),
    ( 1,  1,  1)
  );
begin
  if (imageWidth <= 2) or (imageHeight <= 2) then
    Exit;
    
  SetLength(dstMatrix, imageWidth, imageHeight, 3);
  SetLength(gray, imageWidth, imageHeight);
  
  // Convertir a escala de grises usando luminancia
  for x := 0 to imageWidth - 1 do
    for y := 0 to imageHeight - 1 do
      gray[x, y] := Round(0.299 * srcMatrix[x, y, 0] + 
                          0.587 * srcMatrix[x, y, 1] + 
                          0.114 * srcMatrix[x, y, 2]);
  
  // Aplicar convolución con máscaras de Prewitt
  for x := 1 to imageWidth - 2 do
    for y := 1 to imageHeight - 2 do
    begin
      gx := 0;
      gy := 0;
      
      // Aplicar máscaras 3x3
      for i := -1 to 1 do
        for j := -1 to 1 do
        begin
          gx := gx + (gray[x + i, y + j] * kernelX[i, j]);
          gy := gy + (gray[x + i, y + j] * kernelY[i, j]);
        end;
      
      // Magnitud del gradiente: sqrt(Gx² + Gy²) ≈ |Gx| + |Gy|
      magnitude := Abs(gx) + Abs(gy);
      
      if magnitude > 255 then
        val := 255
      else
        val := Byte(magnitude);
        
      dstMatrix[x, y, 0] := val;
      dstMatrix[x, y, 1] := val;
      dstMatrix[x, y, 2] := val;
    end;
  
  // Rellenar bordes con negro
  for x := 0 to imageWidth - 1 do
  begin
    dstMatrix[x, 0, 0] := 0;
    dstMatrix[x, 0, 1] := 0;
    dstMatrix[x, 0, 2] := 0;
    dstMatrix[x, imageHeight - 1, 0] := 0;
    dstMatrix[x, imageHeight - 1, 1] := 0;
    dstMatrix[x, imageHeight - 1, 2] := 0;
  end;
  for y := 0 to imageHeight - 1 do
  begin
    dstMatrix[0, y, 0] := 0;
    dstMatrix[0, y, 1] := 0;
    dstMatrix[0, y, 2] := 0;
    dstMatrix[imageWidth - 1, y, 0] := 0;
    dstMatrix[imageWidth - 1, y, 1] := 0;
    dstMatrix[imageWidth - 1, y, 2] := 0;
  end;
end;

{ Detección de bordes por Sobel }
procedure EdgeDetectionSobel(imageHeight, imageWidth: Integer; 
  const srcMatrix: RGB_MATRIX; var dstMatrix: RGB_MATRIX);
var
  x, y, i, j: Integer;
  gx, gy, magnitude: Integer;
  gray: array of array of Byte;
  val: Byte;
  // Máscaras de Sobel
  kernelX: array[-1..1, -1..1] of Integer = (
    (-1, 0, 1),
    (-2, 0, 2),
    (-1, 0, 1)
  );
  kernelY: array[-1..1, -1..1] of Integer = (
    (-1, -2, -1),
    ( 0,  0,  0),
    ( 1,  2,  1)
  );
begin
  if (imageWidth <= 2) or (imageHeight <= 2) then
    Exit;
    
  SetLength(dstMatrix, imageWidth, imageHeight, 3);
  SetLength(gray, imageWidth, imageHeight);
  
  // Convertir a escala de grises usando luminancia
  for x := 0 to imageWidth - 1 do
    for y := 0 to imageHeight - 1 do
      gray[x, y] := Round(0.299 * srcMatrix[x, y, 0] + 
                          0.587 * srcMatrix[x, y, 1] + 
                          0.114 * srcMatrix[x, y, 2]);
  
  // Aplicar convolución con máscaras de Sobel
  for x := 1 to imageWidth - 2 do
    for y := 1 to imageHeight - 2 do
    begin
      gx := 0;
      gy := 0;
      
      // Aplicar máscaras 3x3
      for i := -1 to 1 do
        for j := -1 to 1 do
        begin
          gx := gx + (gray[x + i, y + j] * kernelX[i, j]);
          gy := gy + (gray[x + i, y + j] * kernelY[i, j]);
        end;
      
      // Magnitud del gradiente: sqrt(Gx² + Gy²) ≈ |Gx| + |Gy|
      magnitude := Abs(gx) + Abs(gy);
      
      if magnitude > 255 then
        val := 255
      else
        val := Byte(magnitude);
        
      dstMatrix[x, y, 0] := val;
      dstMatrix[x, y, 1] := val;
      dstMatrix[x, y, 2] := val;
    end;
  
  // Rellenar bordes con negro
  for x := 0 to imageWidth - 1 do
  begin
    dstMatrix[x, 0, 0] := 0;
    dstMatrix[x, 0, 1] := 0;
    dstMatrix[x, 0, 2] := 0;
    dstMatrix[x, imageHeight - 1, 0] := 0;
    dstMatrix[x, imageHeight - 1, 1] := 0;
    dstMatrix[x, imageHeight - 1, 2] := 0;
  end;
  for y := 0 to imageHeight - 1 do
  begin
    dstMatrix[0, y, 0] := 0;
    dstMatrix[0, y, 1] := 0;
    dstMatrix[0, y, 2] := 0;
    dstMatrix[imageWidth - 1, y, 0] := 0;
    dstMatrix[imageWidth - 1, y, 1] := 0;
    dstMatrix[imageWidth - 1, y, 2] := 0;
  end;
end;

{ Suavizado recortado (Trimmed Mean Smoothing) }
procedure TrimmedSmoothing(imageHeight, imageWidth: Integer;
  const srcMatrix: RGB_MATRIX; var dstMatrix: RGB_MATRIX;
  maskSize: Integer; trimAmount: Integer);
var
  x, y, c, i, j, idx: Integer;
  halfMask: Integer;
  values: array of Byte;
  count: Integer;
  
  procedure QuickSort(var arr: array of Byte; L, R: Integer);
  var
    I, J: Integer;
    P, T: Byte;
  begin
    repeat
      I := L;
      J := R;
      P := arr[(L + R) shr 1];
      repeat
        while arr[I] < P do Inc(I);
        while arr[J] > P do Dec(J);
        if I <= J then
        begin
          T := arr[I];
          arr[I] := arr[J];
          arr[J] := T;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then QuickSort(arr, L, J);
      L := I;
    until I >= R;
  end;
  
begin
  halfMask := maskSize div 2;
  SetLength(values, maskSize * maskSize);
  
  for x := 0 to imageWidth - 1 do
    for y := 0 to imageHeight - 1 do
      for c := 0 to 2 do
      begin
        count := 0;
        
        // Recolectar valores de la vecindad
        for i := -halfMask to halfMask do
          for j := -halfMask to halfMask do
          begin
            idx := x + i;
            if (idx >= 0) and (idx < imageWidth) then
            begin
              idx := y + j;
              if (idx >= 0) and (idx < imageHeight) then
              begin
                values[count] := srcMatrix[x + i, y + j, c];
                Inc(count);
              end;
            end;
          end;
        
        // Ordenar valores
        if count > 0 then
        begin
          QuickSort(values, 0, count - 1);
          
          // Calcular promedio recortado (sin extremos)
          if count > 2 * trimAmount then
          begin
            idx := 0;
            for i := trimAmount to count - 1 - trimAmount do
              idx := idx + values[i];
            dstMatrix[x, y, c] := idx div (count - 2 * trimAmount);
          end
          else
            dstMatrix[x, y, c] := values[count div 2]; // Mediana si hay pocos valores
        end
        else
          dstMatrix[x, y, c] := srcMatrix[x, y, c];
      end;
  
  SetLength(values, 0);
end;

{ Textura codificada (Local Binary Pattern - LBP) }
procedure EncodedTexture(imageHeight, imageWidth: Integer;
  const srcMatrix: RGB_MATRIX; var dstMatrix: RGB_MATRIX);
var
  x, y, c: Integer;
  grayMatrix: array of array of Byte;
  i, j: Integer;
  center, neighbor: Byte;
  pattern: Integer;
  dx, dy: array[0..7] of Integer;
  
begin
  // Direcciones de los 8 vecinos (sentido horario desde arriba)
  dx[0] := 0;  dy[0] := -1;  // Arriba
  dx[1] := 1;  dy[1] := -1;  // Arriba-derecha
  dx[2] := 1;  dy[2] := 0;   // Derecha
  dx[3] := 1;  dy[3] := 1;   // Abajo-derecha
  dx[4] := 0;  dy[4] := 1;   // Abajo
  dx[5] := -1; dy[5] := 1;   // Abajo-izquierda
  dx[6] := -1; dy[6] := 0;   // Izquierda
  dx[7] := -1; dy[7] := -1;  // Arriba-izquierda
  
  // Convertir a escala de grises
  SetLength(grayMatrix, imageWidth, imageHeight);
  for x := 0 to imageWidth - 1 do
    for y := 0 to imageHeight - 1 do
    begin
      // Promedio RGB para escala de grises
      grayMatrix[x, y] := (srcMatrix[x, y, 0] + srcMatrix[x, y, 1] + srcMatrix[x, y, 2]) div 3;
    end;
  
  // Calcular LBP para cada pixel
  for x := 1 to imageWidth - 2 do
    for y := 1 to imageHeight - 2 do
    begin
      center := grayMatrix[x, y];
      pattern := 0;
      
      // Comparar con los 8 vecinos
      for i := 0 to 7 do
      begin
        neighbor := grayMatrix[x + dx[i], y + dy[i]];
        if neighbor >= center then
          pattern := pattern or (1 shl i);
      end;
      
      // Asignar patrón codificado (normalizar a 0-255)
      for c := 0 to 2 do
        dstMatrix[x, y, c] := pattern;
    end;
  
  // Bordes en negro
  for x := 0 to imageWidth - 1 do
  begin
    for c := 0 to 2 do
    begin
      dstMatrix[x, 0, c] := 0;
      dstMatrix[x, imageHeight - 1, c] := 0;
    end;
  end;
  for y := 0 to imageHeight - 1 do
  begin
    for c := 0 to 2 do
    begin
      dstMatrix[0, y, c] := 0;
      dstMatrix[imageWidth - 1, y, c] := 0;
    end;
  end;
  
  SetLength(grayMatrix, 0, 0);
end;

{ Transformaciones Geométricas }

{ Aumento de escala con factor 2× usando interpolación bilineal }
procedure ScaleUpBilinear(imageHeight, imageWidth: Integer;
  const srcMatrix: RGB_MATRIX; var dstMatrix: RGB_MATRIX;
  out newWidth, newHeight: Integer);
var
  x, y, c: Integer;
  srcX, srcY: Double;
  x1, y1, x2, y2: Integer;
  dx, dy: Double;
  c1, c2, c3, c4: Byte;
begin
  // Calcular nuevas dimensiones (factor 2.0)
  newWidth := imageWidth * 2;
  newHeight := imageHeight * 2;
  
  // Crear matriz para la imagen escalada
  SetLength(dstMatrix, newWidth, newHeight, 3);
  
  // Interpolación bilineal
  for x := 0 to newWidth - 1 do
    for y := 0 to newHeight - 1 do
    begin
      // Coordenadas en la imagen original
      srcX := x / 2.0;
      srcY := y / 2.0;
      
      // Píxeles vecinos
      x1 := Trunc(srcX);
      y1 := Trunc(srcY);
      x2 := x1 + 1;
      y2 := y1 + 1;
      
      // Asegurar límites
      if x2 >= imageWidth then x2 := imageWidth - 1;
      if y2 >= imageHeight then y2 := imageHeight - 1;
      
      // Factores de interpolación
      dx := srcX - x1;
      dy := srcY - y1;
      
      // Interpolar cada canal
      for c := 0 to 2 do
      begin
        c1 := srcMatrix[x1, y1, c];
        c2 := srcMatrix[x2, y1, c];
        c3 := srcMatrix[x1, y2, c];
        c4 := srcMatrix[x2, y2, c];
        
        dstMatrix[x, y, c] := Round(
          c1 * (1 - dx) * (1 - dy) +
          c2 * dx * (1 - dy) +
          c3 * (1 - dx) * dy +
          c4 * dx * dy
        );
      end;
    end;
end;

{ Reducción de escala con factor ½ usando interpolación bilineal }
procedure ScaleDownBilinear(imageHeight, imageWidth: Integer;
  const srcMatrix: RGB_MATRIX; var dstMatrix: RGB_MATRIX;
  out newWidth, newHeight: Integer);
var
  x, y, c: Integer;
  srcX, srcY: Double;
  x1, y1, x2, y2: Integer;
  dx, dy: Double;
  c1, c2, c3, c4: Byte;
begin
  // Calcular nuevas dimensiones (factor 0.5)
  newWidth := imageWidth div 2;
  newHeight := imageHeight div 2;
  
  // Crear matriz para la imagen escalada
  SetLength(dstMatrix, newWidth, newHeight, 3);
  
  // Interpolación bilineal
  for x := 0 to newWidth - 1 do
    for y := 0 to newHeight - 1 do
    begin
      // Coordenadas en la imagen original
      srcX := x * 2.0;
      srcY := y * 2.0;
      
      // Píxeles vecinos
      x1 := Trunc(srcX);
      y1 := Trunc(srcY);
      x2 := x1 + 1;
      y2 := y1 + 1;
      
      // Asegurar límites
      if x2 >= imageWidth then x2 := imageWidth - 1;
      if y2 >= imageHeight then y2 := imageHeight - 1;
      
      // Factores de interpolación
      dx := srcX - x1;
      dy := srcY - y1;
      
      // Interpolar cada canal
      for c := 0 to 2 do
      begin
        c1 := srcMatrix[x1, y1, c];
        c2 := srcMatrix[x2, y1, c];
        c3 := srcMatrix[x1, y2, c];
        c4 := srcMatrix[x2, y2, c];
        
        dstMatrix[x, y, c] := Round(
          c1 * (1 - dx) * (1 - dy) +
          c2 * dx * (1 - dy) +
          c3 * (1 - dx) * dy +
          c4 * dx * dy
        );
      end;
    end;
end;

{ Rotación 90° a la derecha (sentido horario) }
procedure RotateRight90(imageHeight, imageWidth: Integer;
  const srcMatrix: RGB_MATRIX; var dstMatrix: RGB_MATRIX;
  out newWidth, newHeight: Integer);
var
  x, y, c: Integer;
begin
  // Intercambiar dimensiones
  newWidth := imageHeight;
  newHeight := imageWidth;
  
  // Crear matriz para la imagen rotada
  SetLength(dstMatrix, newWidth, newHeight, 3);
  
  // Rotar 90° a la derecha (sentido horario)
  for x := 0 to imageWidth - 1 do
    for y := 0 to imageHeight - 1 do
      for c := 0 to 2 do
        dstMatrix[imageHeight - 1 - y, x, c] := srcMatrix[x, y, c];
end;

{ Rotación 90° a la izquierda (sentido antihorario) }
procedure RotateLeft90(imageHeight, imageWidth: Integer;
  const srcMatrix: RGB_MATRIX; var dstMatrix: RGB_MATRIX;
  out newWidth, newHeight: Integer);
var
  x, y, c: Integer;
begin
  // Intercambiar dimensiones
  newWidth := imageHeight;
  newHeight := imageWidth;
  
  // Crear matriz para la imagen rotada
  SetLength(dstMatrix, newWidth, newHeight, 3);
  
  // Rotar 90° a la izquierda (sentido antihorario)
  for x := 0 to imageWidth - 1 do
    for y := 0 to imageHeight - 1 do
      for c := 0 to 2 do
        dstMatrix[y, imageWidth - 1 - x, c] := srcMatrix[x, y, c];
end;

end.
