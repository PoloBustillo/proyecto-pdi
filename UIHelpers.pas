unit UIHelpers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, StdCtrls, ComCtrls, Dialogs,
  ImageProcessing, FormHistogram;


{ Funciones de utilidad para la interfaz de usuario }
procedure ShowImageHistogram(imageWidth, imageHeight: Integer;
  const matrix: RGB_MATRIX; const hsvMatrix: HSV_MATRIX; colorMode: Integer);
  
procedure CalculateImageCoordinates(imageControl: TControl; imgWidth, imgHeight: Integer;
  mouseX, mouseY: Integer; stretch, proportional, center: Boolean;
  out imgX, imgY: Integer; out isValid: Boolean);

implementation

{ Procedimiento para calcular y mostrar el histograma en un formulario aparte }
procedure ShowImageHistogram(imageWidth, imageHeight: Integer;
  const matrix: RGB_MATRIX; const hsvMatrix: HSV_MATRIX; colorMode: Integer);
var
  histData: THistogramData;
  x, y: Integer;
  r, g, b: Byte;
  h, s, v: Double;
  hByte, sByte, vByte: Byte;
  intensity: Integer;
begin
  if (imageWidth = 0) or (imageHeight = 0) then
  begin
    ShowMessage('Primero debes cargar una imagen');
    Exit;
  end;

  // Crear el formulario de histograma si no existe
  if FormHist = nil then
    FormHist := TFormHist.Create(nil);

  // Inicializar histograma
  for x := 0 to 255 do
  begin
    histData.Red[x]       := 0;
    histData.Green[x]     := 0;
    histData.Blue[x]      := 0;
    histData.Intensity[x] := 0;
  end;

  // Recorrer la imagen y acumular frecuencias según el modo
  if colorMode = 3 then  // Modo HSV
  begin
    // En modo HSV, mostrar histograma de H, S, V
    for x := 0 to imageWidth - 1 do
      for y := 0 to imageHeight - 1 do
      begin
        h := hsvMatrix[x, y, 0];
        s := hsvMatrix[x, y, 1];
        v := hsvMatrix[x, y, 2];
        
        // Convertir a rango 0-255 para el histograma
        hByte := Round(h * 255.0 / 360.0);  // H: 0-360° → 0-255
        sByte := Round(s * 255.0);           // S: 0-1 → 0-255
        vByte := Round(v * 255.0);           // V: 0-1 → 0-255
        
        Inc(histData.Red[hByte]);      // Usar canal Rojo para H (Matiz)
        Inc(histData.Green[sByte]);    // Usar canal Verde para S (Saturación)
        Inc(histData.Blue[vByte]);     // Usar canal Azul para V (Brillo)
        Inc(histData.Intensity[vByte]); // Intensidad = V en HSV
      end;
  end
  else  // Modo RGB
  begin
    for x := 0 to imageWidth - 1 do
      for y := 0 to imageHeight - 1 do
      begin
        r := matrix[x, y, 0];
        g := matrix[x, y, 1];
        b := matrix[x, y, 2];

        // Acumular frecuencias por canal RGB
        Inc(histData.Red[r]);
        Inc(histData.Green[g]);
        Inc(histData.Blue[b]);

        // Calcular intensidad usando la fórmula de luminancia perceptual (ITU-R BT.601)
        // Este método pondera los canales según la sensibilidad del ojo humano
        intensity := Round(0.299 * r + 0.587 * g + 0.114 * b);
        
        // Asegurar que intensity esté en el rango válido [0..255]
        if intensity > 255 then
          intensity := 255
        else if intensity < 0 then
          intensity := 0;
        
        Inc(histData.Intensity[intensity]);
      end;
  end;

  // Mostrar el histograma en el formulario dedicado, pasando el modo de color
  FormHist.ShowHistogram(histData, colorMode);
end;

{ Procedimiento para calcular coordenadas de imagen desde coordenadas del mouse }
procedure CalculateImageCoordinates(imageControl: TControl; imgWidth, imgHeight: Integer;
  mouseX, mouseY: Integer; stretch, proportional, center: Boolean;
  out imgX, imgY: Integer; out isValid: Boolean);
var
  scaleX, scaleY: Double;
  offsetX, offsetY: Integer;
  displayWidth, displayHeight: Integer;
begin
  isValid := False;
  imgX := -1;
  imgY := -1;
  
  // Verificar que haya una imagen cargada
  if (imgWidth = 0) or (imgHeight = 0) then
    Exit;
  
  // Calcular la escala y offset debido a Stretch y Center
  if stretch then
  begin
    if proportional then
    begin
      // Calcular aspecto ratio
      scaleX := imageControl.Width / imgWidth;
      scaleY := imageControl.Height / imgHeight;
      
      // Usar la menor escala para mantener proporciones
      if scaleX < scaleY then
      begin
        displayWidth := imageControl.Width;
        displayHeight := Round(imgHeight * scaleX);
        offsetX := 0;
        offsetY := (imageControl.Height - displayHeight) div 2;
      end
      else
      begin
        displayWidth := Round(imgWidth * scaleY);
        displayHeight := imageControl.Height;
        offsetX := (imageControl.Width - displayWidth) div 2;
        offsetY := 0;
      end;
      
      // Ajustar coordenadas del mouse
      mouseX := mouseX - offsetX;
      mouseY := mouseY - offsetY;
      
      // Convertir a coordenadas de imagen
      if displayWidth > 0 then
        imgX := Round((mouseX * imgWidth) / displayWidth)
      else
        Exit;
        
      if displayHeight > 0 then
        imgY := Round((mouseY * imgHeight) / displayHeight)
      else
        Exit;
    end
    else
    begin
      // Sin proporcional, solo stretch
      imgX := Round((mouseX * imgWidth) / imageControl.Width);
      imgY := Round((mouseY * imgHeight) / imageControl.Height);
    end;
  end
  else
  begin
    // Sin stretch, las coordenadas son directas (considerar center)
    if center then
    begin
      offsetX := (imageControl.Width - imgWidth) div 2;
      offsetY := (imageControl.Height - imgHeight) div 2;
      imgX := mouseX - offsetX;
      imgY := mouseY - offsetY;
    end
    else
    begin
      imgX := mouseX;
      imgY := mouseY;
    end;
  end;
  
  // Verificar que las coordenadas estén dentro de los límites
  isValid := (imgX >= 0) and (imgX < imgWidth) and (imgY >= 0) and (imgY < imgHeight);
end;

end.
