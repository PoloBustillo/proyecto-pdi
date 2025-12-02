unit FormFourier;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Math, ImageProcessing;

type

  { TFormFourierTransform }

  TFormFourierTransform = class(TForm)
    BtnAplicar: TButton;
    BtnCerrar: TButton;
    BtnInversa: TButton;
    CheckFase: TCheckBox;
    CheckMagnitud: TCheckBox;
    EditRadio: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    ImageEspectro: TImage;
    ImageOriginal: TImage;
    ImageResultado: TImage;
    Label1: TLabel;
    Label2: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    RadioDFT: TRadioButton;
    RadioFFT: TRadioButton;
    RadioPasaAltas: TRadioButton;
    RadioPasaBanda: TRadioButton;
    RadioPasaBajas: TRadioButton;
    RadioSinFiltro: TRadioButton;
    TabEspectro: TTabSheet;
    TabOriginal: TTabSheet;
    TabResultado: TTabSheet;
    procedure BtnAplicarClick(Sender: TObject);
    procedure BtnCerrarClick(Sender: TObject);
    procedure BtnInversaClick(Sender: TObject);
  private
    FSourceMatrix: RGB_MATRIX;
    FImageHeight: Integer;
    FImageWidth: Integer;
    FFrequencyReal: array of array of Double;  // Parte real
    FFrequencyImag: array of array of Double;  // Parte imaginaria
    FHasTransform: Boolean;
    
    procedure ApplyFFT;
    procedure ApplyInverseFFT;
    procedure ShowSpectrum;
    procedure ApplyFrequencyFilter;
  public
    procedure SetSourceImage(const srcMatrix: RGB_MATRIX; imgHeight, imgWidth: Integer);
  end;

var
  FormFourierTransform: TFormFourierTransform;

implementation

{$R *.lfm}

{ TFormFourierTransform }

procedure TFormFourierTransform.SetSourceImage(const srcMatrix: RGB_MATRIX; 
  imgHeight, imgWidth: Integer);
var
  bmp: TBitmap;
begin
  FImageHeight := imgHeight;
  FImageWidth := imgWidth;
  SetLength(FSourceMatrix, imgWidth, imgHeight, 3);
  FSourceMatrix := Copy(srcMatrix, 0, Length(srcMatrix));
  FHasTransform := False;
  
  // Mostrar imagen original
  bmp := TBitmap.Create;
  try
    bmp.PixelFormat := pf24bit;
    bmp.SetSize(imgWidth, imgHeight);
    ImageProcessing.CopiarMatrizAImagen(imgHeight, imgWidth, FSourceMatrix, bmp);
    ImageOriginal.Picture.Assign(bmp);
  finally
    bmp.Free;
  end;
end;

procedure TFormFourierTransform.BtnAplicarClick(Sender: TObject);
var
  totalPixels: Integer;
begin
  if (FImageWidth = 0) or (FImageHeight = 0) then
  begin
    ShowMessage('No hay imagen cargada');
    Exit;
  end;
  
  // Advertencia para imágenes grandes
  totalPixels := FImageWidth * FImageHeight;
  if totalPixels > 65536 then // 256x256
  begin
    if MessageDlg('Advertencia', 
      Format('La imagen es grande (%dx%d). La DFT puede tardar varios minutos o congelar la aplicación. ' +
             'Se recomienda usar imágenes más pequeñas (ej: 128x128 o 256x256).'+#13#10#13#10+
             '¿Desea continuar?', [FImageWidth, FImageHeight]),
      mtWarning, [mbYes, mbNo], 0) <> mrYes then
      Exit;
  end;
  
  Screen.Cursor := crHourGlass;
  BtnAplicar.Enabled := False;
  try
    ApplyFFT;
    ShowSpectrum;
    ApplyFrequencyFilter;
    
    BtnInversa.Enabled := True;
    PageControl1.ActivePage := TabEspectro;
  finally
    Screen.Cursor := crDefault;
    BtnAplicar.Enabled := True;
  end;
end;

procedure TFormFourierTransform.BtnCerrarClick(Sender: TObject);
begin
  Close;
end;

procedure TFormFourierTransform.BtnInversaClick(Sender: TObject);
begin
  if not FHasTransform then
  begin
    ShowMessage('Primero debes aplicar la FFT');
    Exit;
  end;
  
  Screen.Cursor := crHourGlass;
  BtnInversa.Enabled := False;
  try
    ApplyInverseFFT;
    PageControl1.ActivePage := TabResultado;
  finally
    Screen.Cursor := crDefault;
    BtnInversa.Enabled := True;
  end;
end;

procedure TFormFourierTransform.ApplyFFT;
var
  x, y: Integer;
  grayMatrix: array of array of Byte;
  u, v: Integer;
  sumReal, sumImag: Double;
  angle: Double;
  gray: Byte;
begin
  // Convertir a escala de grises
  SetLength(grayMatrix, FImageWidth, FImageHeight);
  for x := 0 to FImageWidth - 1 do
    for y := 0 to FImageHeight - 1 do
    begin
      gray := Round(0.299 * FSourceMatrix[x, y, 0] + 
                    0.587 * FSourceMatrix[x, y, 1] + 
                    0.114 * FSourceMatrix[x, y, 2]);
      grayMatrix[x, y] := gray;
    end;
  
  // Inicializar matrices de frecuencia
  SetLength(FFrequencyReal, FImageWidth, FImageHeight);
  SetLength(FFrequencyImag, FImageWidth, FImageHeight);
  
  // Aplicar DFT 2D (versión simplificada para demostración)
  // En producción, se debería usar FFT optimizada (Cooley-Tukey)
  // ADVERTENCIA: Complejidad O(N^4) - muy lento para imágenes grandes
  for u := 0 to FImageWidth - 1 do
  begin
    for v := 0 to FImageHeight - 1 do
    begin
      sumReal := 0;
      sumImag := 0;
      
      for x := 0 to FImageWidth - 1 do
        for y := 0 to FImageHeight - 1 do
        begin
          angle := -2 * Pi * ((u * x / FImageWidth) + (v * y / FImageHeight));
          sumReal := sumReal + grayMatrix[x, y] * Cos(angle);
          sumImag := sumImag + grayMatrix[x, y] * Sin(angle);
        end;
      
      FFrequencyReal[u, v] := sumReal;
      FFrequencyImag[u, v] := sumImag;
    end;
    
    // Actualizar interfaz cada fila procesada
    if (u mod Max(1, FImageWidth div 20)) = 0 then
    begin
      Application.ProcessMessages;
      Caption := Format('Aplicando FFT... %d%%', [Round(100 * u / FImageWidth)]);
    end;
  end;
  
  Caption := 'Transformada de Fourier';
  
  FHasTransform := True;
  SetLength(grayMatrix, 0, 0);
end;

procedure TFormFourierTransform.ShowSpectrum;
var
  x, y: Integer;
  magnitude, maxMag, minMag: Double;
  phase: Double;
  magnitudeMatrix: array of array of Double;
  scaledValue: Byte;
  bmp: TBitmap;
  resultMatrix: RGB_MATRIX;
  centerX, centerY: Integer;
  shiftedX, shiftedY: Integer;
begin
  if not FHasTransform then Exit;
  
  SetLength(magnitudeMatrix, FImageWidth, FImageHeight);
  SetLength(resultMatrix, FImageWidth, FImageHeight, 3);
  
  // Calcular magnitud (espectro de potencia)
  maxMag := 0;
  minMag := 1e30;
  
  for x := 0 to FImageWidth - 1 do
    for y := 0 to FImageHeight - 1 do
    begin
      magnitude := Sqrt(Sqr(FFrequencyReal[x, y]) + Sqr(FFrequencyImag[x, y]));
      
      // Usar escala logarítmica para mejor visualización
      if magnitude > 0 then
        magnitude := Ln(1 + magnitude);
      
      magnitudeMatrix[x, y] := magnitude;
      
      if magnitude > maxMag then maxMag := magnitude;
      if magnitude < minMag then minMag := magnitude;
    end;
  
  // Normalizar y aplicar shift (centrar componente DC)
  centerX := FImageWidth div 2;
  centerY := FImageHeight div 2;
  
  for x := 0 to FImageWidth - 1 do
    for y := 0 to FImageHeight - 1 do
    begin
      // Aplicar fftshift: mover DC al centro
      shiftedX := (x + centerX) mod FImageWidth;
      shiftedY := (y + centerY) mod FImageHeight;
      
      // Normalizar a 0-255
      if maxMag > minMag then
        scaledValue := Round(255 * (magnitudeMatrix[x, y] - minMag) / (maxMag - minMag))
      else
        scaledValue := 0;
      
      resultMatrix[shiftedX, shiftedY, 0] := scaledValue;
      resultMatrix[shiftedX, shiftedY, 1] := scaledValue;
      resultMatrix[shiftedX, shiftedY, 2] := scaledValue;
    end;
  
  // Mostrar espectro
  bmp := TBitmap.Create;
  try
    bmp.PixelFormat := pf24bit;
    bmp.SetSize(FImageWidth, FImageHeight);
    ImageProcessing.CopiarMatrizAImagen(FImageHeight, FImageWidth, resultMatrix, bmp);
    ImageEspectro.Picture.Assign(bmp);
  finally
    bmp.Free;
  end;
  
  SetLength(magnitudeMatrix, 0, 0);
  SetLength(resultMatrix, 0, 0, 0);
end;

procedure TFormFourierTransform.ApplyFrequencyFilter;
var
  x, y: Integer;
  centerX, centerY: Integer;
  distance: Double;
  radius: Integer;
begin
  if not FHasTransform then Exit;
  if RadioSinFiltro.Checked then Exit;
  
  // Obtener radio del filtro
  radius := StrToIntDef(EditRadio.Text, 30);
  if radius < 1 then radius := 30;
  
  centerX := FImageWidth div 2;
  centerY := FImageHeight div 2;
  
  // Aplicar filtro en el dominio de frecuencia
  for x := 0 to FImageWidth - 1 do
    for y := 0 to FImageHeight - 1 do
    begin
      distance := Sqrt(Sqr(x - centerX) + Sqr(y - centerY));
      
      if RadioPasaBajas.Checked then
      begin
        // Filtro pasa bajas: eliminar frecuencias altas
        if distance > radius then
        begin
          FFrequencyReal[x, y] := 0;
          FFrequencyImag[x, y] := 0;
        end;
      end
      else if RadioPasaAltas.Checked then
      begin
        // Filtro pasa altas: eliminar frecuencias bajas
        if distance < radius then
        begin
          FFrequencyReal[x, y] := 0;
          FFrequencyImag[x, y] := 0;
        end;
      end
      else if RadioPasaBanda.Checked then
      begin
        // Filtro pasa banda: mantener solo un rango
        if (distance < radius * 0.5) or (distance > radius * 1.5) then
        begin
          FFrequencyReal[x, y] := 0;
          FFrequencyImag[x, y] := 0;
        end;
      end;
    end;
end;

procedure TFormFourierTransform.ApplyInverseFFT;
var
  x, y: Integer;
  u, v: Integer;
  sumReal: Double;
  angle: Double;
  pixelValue: Double;
  resultMatrix: RGB_MATRIX;
  bmp: TBitmap;
  minVal, maxVal: Double;
  normalizedValue: Byte;
  spatialMatrix: array of array of Double;
begin
  if not FHasTransform then Exit;
  
  SetLength(resultMatrix, FImageWidth, FImageHeight, 3);
  SetLength(spatialMatrix, FImageWidth, FImageHeight);
  
  // Aplicar IDFT 2D - Calcular una sola vez
  minVal := 1e30;
  maxVal := -1e30;
  
  for x := 0 to FImageWidth - 1 do
  begin
    for y := 0 to FImageHeight - 1 do
    begin
      sumReal := 0;
      
      for u := 0 to FImageWidth - 1 do
        for v := 0 to FImageHeight - 1 do
        begin
          angle := 2 * Pi * ((u * x / FImageWidth) + (v * y / FImageHeight));
          sumReal := sumReal + 
            FFrequencyReal[u, v] * Cos(angle) - 
            FFrequencyImag[u, v] * Sin(angle);
        end;
      
      pixelValue := sumReal / (FImageWidth * FImageHeight);
      spatialMatrix[x, y] := pixelValue;
      
      if pixelValue < minVal then minVal := pixelValue;
      if pixelValue > maxVal then maxVal := pixelValue;
    end;
    
    // Actualizar interfaz cada fila procesada
    if (x mod Max(1, FImageWidth div 20)) = 0 then
    begin
      Application.ProcessMessages;
      Caption := Format('Aplicando FFT Inversa... %d%%', [Round(100 * x / FImageWidth)]);
    end;
  end;
  
  Caption := 'Transformada de Fourier';
  
  // Normalizar valores a 0-255
  for x := 0 to FImageWidth - 1 do
    for y := 0 to FImageHeight - 1 do
    begin
      pixelValue := spatialMatrix[x, y];
      
      if maxVal > minVal then
        normalizedValue := Round(255 * (pixelValue - minVal) / (maxVal - minVal))
      else
        normalizedValue := 0;
      
      resultMatrix[x, y, 0] := normalizedValue;
      resultMatrix[x, y, 1] := normalizedValue;
      resultMatrix[x, y, 2] := normalizedValue;
    end;
  
  // Mostrar resultado
  bmp := TBitmap.Create;
  try
    bmp.PixelFormat := pf24bit;
    bmp.SetSize(FImageWidth, FImageHeight);
    ImageProcessing.CopiarMatrizAImagen(FImageHeight, FImageWidth, resultMatrix, bmp);
    ImageResultado.Picture.Assign(bmp);
  finally
    bmp.Free;
  end;
  
  SetLength(spatialMatrix, 0, 0);
  SetLength(resultMatrix, 0, 0, 0);
end;

end.
