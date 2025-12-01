unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  ExtCtrls, StdCtrls, ExtDlgs, TAGraph, TASeries, TAChartUtils, Math,
  ImageProcessing, FormHistogram, FormBinarize, FormGamma;

type

  { TForm1 }

  TForm1 = class(TForm)
    Image1: TImage;
    MainMenu1: TMainMenu;
    Abrir: TMenuItem;
    Guardar: TMenuItem;
    Archivo: TMenuItem;
    Gamma: TMenuItem;
    Contraste: TMenuItem;
    HSV: TMenuItem;
    Grises: TMenuItem;
    MenuItem1: TMenuItem;
    ContrasteReducir: TMenuItem;
    Transformaciones: TMenuItem;
    EscalaMas: TMenuItem;
    EscalaMenos: TMenuItem;
    RotacionIzq: TMenuItem;
    RotacionDer: TMenuItem;
    Restart: TMenuItem;
    Restaurar: TMenuItem;
    MenuItem11: TMenuItem;
    Graficos: TMenuItem;
    Histograma: TMenuItem;
    Bordes: TMenuItem;
    FiltrosColor: TMenuItem;
    Binarizacion: TMenuItem;
    Diferencia: TMenuItem;
    Sobel: TMenuItem;
    Prewitt: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;

    procedure ContrasteReducirClick(Sender: TObject);
    procedure DiferenciaClick(Sender: TObject);
    procedure EscalaMenosClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure AbrirClick(Sender: TObject);
    procedure GrisesClick(Sender: TObject);
    procedure GuardarClick(Sender: TObject);
    procedure HSVClick(Sender: TObject);
    procedure BinarizacionClick(Sender: TObject);
    procedure GammaClick(Sender: TObject);
    procedure HistogramaClick(Sender: TObject);
    procedure ContrasteClick(Sender: TObject);
    procedure EscalaMasClick(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure PrewittClick(Sender: TObject);
    procedure RestaurarClick(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure RotacionDerClick(Sender: TObject);
    procedure RotacionIzqClick(Sender: TObject);
    procedure SobelClick(Sender: TObject);
  end;

var
  Form1: TForm1;
  IMG_HEIGHT, IMG_WIDTH, COLOR_MODE: Integer;
  MATRIX, ORIGINAL_MATRIX: RGB_MATRIX;
  CONVERTED_HSV_MATRIX: HSV_MATRIX;
  GRAY_SCALE_VALUES: GRAY_SCALE_MATRIX;
  CONVERTED_GRAY_MATRIX: RGB_MATRIX;
  BMAP: TBitmap;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  BMAP := TBitmap.Create;
  Image1.Stretch := True;
  Image1.Proportional := True;
  Image1.Center := True;
  
  // Inicializar StatusBar
  if StatusBar1.Panels.Count > 0 then
    StatusBar1.Panels[0].Text := 'Modo: RGB';
end;

procedure TForm1.DiferenciaClick(Sender: TObject);
var
  resultMatrix: RGB_MATRIX;
begin
  if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then
  begin
    ShowMessage('Primero debes cargar una imagen');
    Exit;
  end;

  // Crear matriz para el resultado
  SetLength(resultMatrix, IMG_WIDTH, IMG_HEIGHT, 3);
  
  // Aplicar detección de bordes por diferencia
  ImageProcessing.EdgeDetectionDifference(IMG_HEIGHT, IMG_WIDTH, MATRIX, resultMatrix);
  
  // Actualizar la matriz y la imagen
  MATRIX := resultMatrix;
  ImageProcessing.CopyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, MATRIX, BMAP);
  Image1.Picture.Assign(BMAP);
  
  // Sincronizar matriz HSV
  ImageProcessing.RGBMatrixToHSVMatrix(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_HSV_MATRIX);
end;

procedure TForm1.EscalaMenosClick(Sender: TObject);
var
  newWidth, newHeight: Integer;
  newMatrix: RGB_MATRIX;
  x, y, c: Integer;
  srcX, srcY: Double;
  x1, y1, x2, y2: Integer;
  dx, dy: Double;
  c1, c2, c3, c4: Byte;
begin
  if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then
  begin
    ShowMessage('Primero debes cargar una imagen');
    Exit;
  end;

  // Calcular nuevas dimensiones (factor 0.5)
  newWidth := IMG_WIDTH div 2;
  newHeight := IMG_HEIGHT div 2;
  
  if (newWidth < 10) or (newHeight < 10) then
  begin
    ShowMessage('La imagen es demasiado pequeña para reducir más');
    Exit;
  end;

  // Crear matriz para la imagen escalada
  SetLength(newMatrix, newWidth, newHeight, 3);
  
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
      if x2 >= IMG_WIDTH then x2 := IMG_WIDTH - 1;
      if y2 >= IMG_HEIGHT then y2 := IMG_HEIGHT - 1;
      
      // Factores de interpolación
      dx := srcX - x1;
      dy := srcY - y1;
      
      // Interpolar cada canal
      for c := 0 to 2 do
      begin
        c1 := MATRIX[x1, y1, c];
        c2 := MATRIX[x2, y1, c];
        c3 := MATRIX[x1, y2, c];
        c4 := MATRIX[x2, y2, c];
        
        newMatrix[x, y, c] := Round(
          c1 * (1 - dx) * (1 - dy) +
          c2 * dx * (1 - dy) +
          c3 * (1 - dx) * dy +
          c4 * dx * dy
        );
      end;
    end;
  
  // Actualizar dimensiones e imagen
  IMG_WIDTH := newWidth;
  IMG_HEIGHT := newHeight;
  MATRIX := newMatrix;
  
  BMAP.SetSize(IMG_WIDTH, IMG_HEIGHT);
  ImageProcessing.CopyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, MATRIX, BMAP);
  Image1.Picture.Assign(BMAP);
  
  // Actualizar StatusBar
  StatusBar1.Panels[6].Text := IntToStr(IMG_HEIGHT) + 'x' + IntToStr(IMG_WIDTH);
  
  // Sincronizar matriz HSV
  ImageProcessing.RGBMatrixToHSVMatrix(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_HSV_MATRIX);
end;

procedure TForm1.ContrasteReducirClick(Sender: TObject);
var
  x, y, c: Integer;
  value: Byte;
  newValue: Integer;
begin
  if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then
  begin
    ShowMessage('Primero debes cargar una imagen');
    Exit;
  end;

  // Reducir contraste: comprimir valores al rango [64, 192]
  for x := 0 to IMG_WIDTH - 1 do
    for y := 0 to IMG_HEIGHT - 1 do
      for c := 0 to 2 do
      begin
        value := MATRIX[x, y, c];
        // Mapear [0,255] → [64,192]
        newValue := 64 + Round((value * 128) / 255);
        MATRIX[x, y, c] := Byte(newValue);
      end;

  ImageProcessing.CopyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, MATRIX, BMAP);
  Image1.Picture.Assign(BMAP);

  // Sincronizar matriz HSV
  ImageProcessing.RGBMatrixToHSVMatrix(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_HSV_MATRIX);
end;

procedure TForm1.AbrirClick(Sender: TObject);
begin
  // Configurar el diálogo antes de abrirlo
  OpenDialog1.Title := 'Seleccionar imagen';
  OpenDialog1.Filter := 'Archivos de imagen|*.bmp;*.jpg;*.jpeg;*.png|Bitmap (*.bmp)|*.bmp;*.BMP|Todos los archivos|*.*';
  OpenDialog1.FilterIndex := 1;
  OpenDialog1.InitialDir := ExtractFilePath(Application.ExeName);
  OpenDialog1.Options := [ofFileMustExist, ofEnableSizing, ofViewDetail];

  if OpenDialog1.Execute then
  begin
    SetLength(MATRIX, 0, 0, 0);
    SetLength(ORIGINAL_MATRIX, 0, 0, 0);
    SetLength(CONVERTED_GRAY_MATRIX, 0, 0, 0);
    SetLength(CONVERTED_HSV_MATRIX, 0, 0, 0);
    Image1.Enabled := True;
    BMAP.LoadFromFile(OpenDialog1.FileName);
    IMG_HEIGHT := BMAP.Height;
    IMG_WIDTH := BMAP.Width;
    if BMAP.PixelFormat <> pf24bit then
      BMAP.PixelFormat := pf24bit;
    StatusBar1.Panels[6].Text := IntToStr(IMG_HEIGHT) + 'x' + IntToStr(IMG_WIDTH);
    SetLength(MATRIX, IMG_WIDTH, IMG_HEIGHT, 3);
    SetLength(ORIGINAL_MATRIX, IMG_WIDTH, IMG_HEIGHT, 3);
    
    // Usar funciones del módulo ImageProcessing
    ImageProcessing.CopyImageToMatrix(IMG_HEIGHT, IMG_WIDTH, BMAP, MATRIX);
    ImageProcessing.CopyImageToMatrix(IMG_HEIGHT, IMG_WIDTH, BMAP, ORIGINAL_MATRIX);
    Image1.Picture.Assign(BMAP);
    ImageProcessing.RGBMatrixToHSVMatrix(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_HSV_MATRIX);
    COLOR_MODE := 1;
    StatusBar1.Panels[0].Text := 'Modo: RGB';
    HSV.Caption := 'Cambiar a HSV';
  end;
end;

procedure TForm1.GrisesClick(Sender: TObject);
begin
  if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then
  begin
    ShowMessage('Primero debes cargar una imagen');
    Exit;
  end;

  // Convertir a escala de grises según el modo actual
  if COLOR_MODE = 3 then  // Modo HSV
  begin
    // Aplicar escala de grises en HSV
    ImageProcessing.HSVToGrayScale(IMG_HEIGHT, IMG_WIDTH, CONVERTED_HSV_MATRIX);
    
    // Convertir HSV a RGB en MATRIX para visualización en monitor
    ImageProcessing.HSVMatrixToRGBMatrix(IMG_HEIGHT, IMG_WIDTH, CONVERTED_HSV_MATRIX, MATRIX);
    ImageProcessing.CopyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, MATRIX, BMAP);
    Image1.Picture.Assign(BMAP);
    
    ShowMessage('Escala de grises aplicada en modo HSV (S=0, preservando V)');
  end
  else  // Modo RGB
  begin
    // Usar función del módulo ImageProcessing para RGB
    ImageProcessing.MediumRangeGrayScale(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_GRAY_MATRIX);
    ImageProcessing.CopyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, CONVERTED_GRAY_MATRIX, BMAP);
    Image1.Picture.Assign(BMAP);
    COLOR_MODE := 2;
  end;
end;

procedure TForm1.GuardarClick(Sender: TObject);
var
  extension: string;
begin
  // Verificar que haya una imagen cargada
  if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then
  begin
    ShowMessage('No hay imagen para guardar. Primero debes cargar una imagen.');
    Exit;
  end;

  // Configurar el diálogo de guardar
  SaveDialog1.Title := 'Guardar imagen como';
  SaveDialog1.Filter := 'Bitmap (*.bmp)|*.bmp|JPEG (*.jpg)|*.jpg|PNG (*.png)|*.png|Todos los archivos|*.*';
  SaveDialog1.FilterIndex := 1;
  SaveDialog1.DefaultExt := 'bmp';
  SaveDialog1.Options := [ofOverwritePrompt, ofEnableSizing, ofViewDetail];
  
  // Si el usuario ya había abierto un archivo, usar su nombre como sugerencia
  if OpenDialog1.FileName <> '' then
    SaveDialog1.FileName := ChangeFileExt(ExtractFileName(OpenDialog1.FileName), '_editado.bmp')
  else
    SaveDialog1.FileName := 'imagen_editada.bmp';

  // Mostrar el diálogo y guardar si el usuario confirma
  if SaveDialog1.Execute then
  begin
    try
      // Convertir a RGB para guardar (los archivos de imagen siempre son RGB)
      if COLOR_MODE = 3 then
      begin
        // En modo HSV: convertir CONVERTED_HSV_MATRIX a MATRIX (RGB) para guardar
        ImageProcessing.HSVMatrixToRGBMatrix(IMG_HEIGHT, IMG_WIDTH, CONVERTED_HSV_MATRIX, MATRIX);
      end;
      
      // Actualizar BMAP con MATRIX (siempre RGB en este punto)
      ImageProcessing.CopyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, MATRIX, BMAP);
      
      // Obtener la extensión del archivo
      extension := LowerCase(ExtractFileExt(SaveDialog1.FileName));
      
      // Guardar según el formato seleccionado
      if (extension = '.bmp') then
      begin
        BMAP.SaveToFile(SaveDialog1.FileName);
      end
      else if (extension = '.jpg') or (extension = '.jpeg') then
      begin
        // Para JPEG, crear un objeto temporal
        with TJPEGImage.Create do
        try
          Assign(BMAP);
          SaveToFile(SaveDialog1.FileName);
        finally
          Free;
        end;
      end
      else if (extension = '.png') then
      begin
        // Para PNG, crear un objeto temporal
        with TPortableNetworkGraphic.Create do
        try
          Assign(BMAP);
          SaveToFile(SaveDialog1.FileName);
        finally
          Free;
        end;
      end
      else
      begin
        // Por defecto, guardar como BMP
        BMAP.SaveToFile(SaveDialog1.FileName);
      end;
      
      ShowMessage('Imagen guardada exitosamente en: ' + SaveDialog1.FileName);
    except
      on E: Exception do
        ShowMessage('Error al guardar la imagen: ' + E.Message);
    end;
  end;
end;

procedure TForm1.HSVClick(Sender: TObject);
begin
  if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then
  begin
    ShowMessage('Primero debes cargar una imagen');
    Exit;
  end;

  if COLOR_MODE = 3 then
  begin
    // Convertir HSV de vuelta a RGB (CONVERTED_HSV_MATRIX → MATRIX)
    ImageProcessing.HSVMatrixToRGBMatrix(IMG_HEIGHT, IMG_WIDTH, CONVERTED_HSV_MATRIX, MATRIX);
    ImageProcessing.CopyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, MATRIX, BMAP);
    Image1.Picture.Assign(BMAP);
    COLOR_MODE := 1;
    StatusBar1.Panels[0].Text := 'Modo: RGB';
    HSV.Caption := 'Cambiar a HSV';
    ShowMessage('Imagen convertida a modo RGB');
  end
  else
  begin
    // Convertir RGB a HSV (MATRIX → CONVERTED_HSV_MATRIX)
    ImageProcessing.RGBMatrixToHSVMatrix(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_HSV_MATRIX);
    
    // Actualizar MATRIX con la representación RGB para display (monitor requiere RGB)
    ImageProcessing.HSVMatrixToRGBMatrix(IMG_HEIGHT, IMG_WIDTH, CONVERTED_HSV_MATRIX, MATRIX);
    ImageProcessing.CopyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, MATRIX, BMAP);
    Image1.Picture.Assign(BMAP);
    
    COLOR_MODE := 3;
    StatusBar1.Panels[0].Text := 'Modo: HSV';
    HSV.Caption := 'Cambiar a RGB';
    ShowMessage('Imagen convertida a modo HSV. Los filtros ahora operan en espacio HSV.');
  end;
end;

procedure TForm1.BinarizacionClick(Sender: TObject);
var
  BinarizeForm: TFormBinarization;
begin
  if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then
  begin
    ShowMessage('Primero debes cargar una imagen');
    Exit;
  end;

  // Crear y mostrar el formulario de binarización
  BinarizeForm := TFormBinarization.Create(Self);
  try
    // En modo HSV, convertir a RGB temporalmente para el formulario
    if COLOR_MODE = 3 then
    begin
      ImageProcessing.HSVMatrixToRGBMatrix(IMG_HEIGHT, IMG_WIDTH, CONVERTED_HSV_MATRIX, MATRIX);
      BinarizeForm.SetSourceImage(MATRIX, IMG_HEIGHT, IMG_WIDTH);
    end
    else
    begin
      // Pasar la imagen actual al formulario (ya en RGB)
      BinarizeForm.SetSourceImage(MATRIX, IMG_HEIGHT, IMG_WIDTH);
    end;

    // Si el usuario hace clic en "Aplicar" (mrOk), actualizar la matriz
    if BinarizeForm.ShowModal = mrOk then
    begin
      // Obtener el resultado en MATRIX (siempre RGB del formulario)
      MATRIX := BinarizeForm.GetResultMatrix;
      
      if COLOR_MODE = 3 then
      begin
        // En modo HSV, convertir resultado RGB de vuelta a HSV
        ImageProcessing.RGBMatrixToHSVMatrix(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_HSV_MATRIX);
        // Actualizar MATRIX con representación RGB para visualización
        ImageProcessing.HSVMatrixToRGBMatrix(IMG_HEIGHT, IMG_WIDTH, CONVERTED_HSV_MATRIX, MATRIX);
      end
      else
      begin
        // En modo RGB, sincronizar CONVERTED_HSV_MATRIX
        ImageProcessing.RGBMatrixToHSVMatrix(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_HSV_MATRIX);
      end;
      
      ImageProcessing.CopyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, MATRIX, BMAP);
      Image1.Picture.Assign(BMAP);
    end;
  finally
    BinarizeForm.Free;
  end;
end;

procedure TForm1.GammaClick(Sender: TObject);
var
  GammaForm: TFormGammaCorrection;
begin
  if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then
  begin
    ShowMessage('Primero debes cargar una imagen');
    Exit;
  end;

  // Crear y mostrar el formulario de corrección gamma
  GammaForm := TFormGammaCorrection.Create(Self);
  try
    // Preparar imagen RGB para el formulario
    if COLOR_MODE = 3 then
    begin
      // En modo HSV: convertir a RGB para el formulario
      ImageProcessing.HSVMatrixToRGBMatrix(IMG_HEIGHT, IMG_WIDTH, CONVERTED_HSV_MATRIX, MATRIX);
      GammaForm.SetSourceImage(MATRIX, IMG_HEIGHT, IMG_WIDTH);
      
      ShowMessage('En modo HSV, la corrección Gamma se aplicará al canal V (Brillo) solamente, ' +
                  'preservando H (Matiz) y S (Saturación).');
    end
    else
    begin
      // Pasar la imagen actual al formulario (ya en RGB)
      GammaForm.SetSourceImage(MATRIX, IMG_HEIGHT, IMG_WIDTH);
    end;

    // Si el usuario hace clic en "Aplicar" (mrOk), actualizar la matriz
    if GammaForm.ShowModal = mrOk then
    begin
      // Obtener resultado en MATRIX
      MATRIX := GammaForm.GetResultMatrix;
      
      if COLOR_MODE = 3 then
      begin
        // En modo HSV: aplicar gamma solo al canal V, preservando H y S
        ImageProcessing.ApplyGammaToHSVValue(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_HSV_MATRIX);
        
        // Actualizar MATRIX con representación RGB para visualización
        ImageProcessing.HSVMatrixToRGBMatrix(IMG_HEIGHT, IMG_WIDTH, CONVERTED_HSV_MATRIX, MATRIX);
      end
      else
      begin
        // En modo RGB: sincronizar CONVERTED_HSV_MATRIX
        ImageProcessing.RGBMatrixToHSVMatrix(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_HSV_MATRIX);
      end;
      
      ImageProcessing.CopyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, MATRIX, BMAP);
      Image1.Picture.Assign(BMAP);
    end;
  finally
    GammaForm.Free;
  end;
end;

// Procedimiento auxiliar para calcular y mostrar el histograma en un formulario aparte
procedure ShowImageHistogram;
var
  histData: THistogramData;
  x, y: Integer;
  r, g, b: Byte;
  h, s, v: Double;
  hByte, sByte, vByte: Byte;
  intensity: Integer;
begin
  if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then
  begin
    ShowMessage('Primero debes cargar una imagen');
    Exit;
  end;

  // Crear el formulario de histograma si no existe
  if FormHist = nil then
    FormHist := TFormHist.Create(Application);

  // Inicializar histograma
  for x := 0 to 255 do
  begin
    histData.Red[x]       := 0;
    histData.Green[x]     := 0;
    histData.Blue[x]      := 0;
    histData.Intensity[x] := 0;
  end;

  // Recorrer la imagen y acumular frecuencias según el modo
  if COLOR_MODE = 3 then  // Modo HSV
  begin
    // En modo HSV, mostrar histograma de H, S, V
    for x := 0 to IMG_WIDTH - 1 do
      for y := 0 to IMG_HEIGHT - 1 do
      begin
        h := CONVERTED_HSV_MATRIX[x, y, 0];
        s := CONVERTED_HSV_MATRIX[x, y, 1];
        v := CONVERTED_HSV_MATRIX[x, y, 2];
        
        // Convertir a rango 0-255 para el histograma
        hByte := Round(h * 255.0 / 360.0);  // H: 0-360° → 0-255
        sByte := Round(s * 255.0);           // S: 0-1 → 0-255
        vByte := Round(v * 255.0);           // V: 0-1 → 0-255
        
        Inc(histData.Red[hByte]);      // Usar canal Rojo para H (Matiz)
        Inc(histData.Green[sByte]);    // Usar canal Verde para S (Saturación)
        Inc(histData.Blue[vByte]);     // Usar canal Azul para V (Brillo)
        Inc(histData.Intensity[vByte]); // Intensidad = V en HSV
      end;
    
    ShowMessage('Histograma HSV: Rojo=H (Matiz), Verde=S (Saturación), Azul=V (Brillo)');
  end
  else  // Modo RGB
  begin
    for x := 0 to IMG_WIDTH - 1 do
      for y := 0 to IMG_HEIGHT - 1 do
      begin
        r := MATRIX[x, y, 0];
        g := MATRIX[x, y, 1];
        b := MATRIX[x, y, 2];

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

  // Mostrar el histograma en el formulario dedicado
  FormHist.ShowHistogram(histData);
end;

procedure TForm1.HistogramaClick(Sender: TObject);
begin
  ShowImageHistogram;
end;

procedure TForm1.ContrasteClick(Sender: TObject);
begin
  if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then
  begin
    ShowMessage('Primero debes cargar una imagen');
    Exit;
  end;

  // Aplicar contraste automático según el modo actual
  if COLOR_MODE = 3 then  // Modo HSV
  begin
    // Aplicar contraste automático solo al canal V (brillo)
    ImageProcessing.AutoContrastHSV(IMG_HEIGHT, IMG_WIDTH, CONVERTED_HSV_MATRIX);
    
    // Convertir HSV a RGB para visualización
    ImageProcessing.HSVMatrixToRGBMatrix(IMG_HEIGHT, IMG_WIDTH, CONVERTED_HSV_MATRIX, MATRIX);
    ImageProcessing.CopyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, MATRIX, BMAP);
    Image1.Picture.Assign(BMAP);
  end
  else  // Modo RGB
  begin
    // Aplicar contraste automático a cada canal RGB independientemente
    ImageProcessing.AutoContrast(IMG_HEIGHT, IMG_WIDTH, MATRIX);
    ImageProcessing.CopyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, MATRIX, BMAP);
    Image1.Picture.Assign(BMAP);
    
    // Sincronizar matriz HSV
    ImageProcessing.RGBMatrixToHSVMatrix(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_HSV_MATRIX);
  end;
end;

procedure TForm1.EscalaMasClick(Sender: TObject);
var
  newWidth, newHeight: Integer;
  newMatrix: RGB_MATRIX;
  x, y, c: Integer;
  srcX, srcY: Double;
  x1, y1, x2, y2: Integer;
  dx, dy: Double;
  c1, c2, c3, c4: Byte;
begin
  if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then
  begin
    ShowMessage('Primero debes cargar una imagen');
    Exit;
  end;

  // Calcular nuevas dimensiones (factor 2.0)
  newWidth := IMG_WIDTH * 2;
  newHeight := IMG_HEIGHT * 2;
  
  if (newWidth > 8000) or (newHeight > 8000) then
  begin
    ShowMessage('La imagen resultante sería demasiado grande (límite: 8000x8000)');
    Exit;
  end;

  // Crear matriz para la imagen escalada
  SetLength(newMatrix, newWidth, newHeight, 3);
  
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
      if x2 >= IMG_WIDTH then x2 := IMG_WIDTH - 1;
      if y2 >= IMG_HEIGHT then y2 := IMG_HEIGHT - 1;
      
      // Factores de interpolación
      dx := srcX - x1;
      dy := srcY - y1;
      
      // Interpolar cada canal
      for c := 0 to 2 do
      begin
        c1 := MATRIX[x1, y1, c];
        c2 := MATRIX[x2, y1, c];
        c3 := MATRIX[x1, y2, c];
        c4 := MATRIX[x2, y2, c];
        
        newMatrix[x, y, c] := Round(
          c1 * (1 - dx) * (1 - dy) +
          c2 * dx * (1 - dy) +
          c3 * (1 - dx) * dy +
          c4 * dx * dy
        );
      end;
    end;
  
  // Actualizar dimensiones e imagen
  IMG_WIDTH := newWidth;
  IMG_HEIGHT := newHeight;
  MATRIX := newMatrix;
  
  BMAP.SetSize(IMG_WIDTH, IMG_HEIGHT);
  ImageProcessing.CopyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, MATRIX, BMAP);
  Image1.Picture.Assign(BMAP);
  
  // Actualizar StatusBar
  StatusBar1.Panels[6].Text := IntToStr(IMG_HEIGHT) + 'x' + IntToStr(IMG_WIDTH);
  
  // Sincronizar matriz HSV
  ImageProcessing.RGBMatrixToHSVMatrix(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_HSV_MATRIX);
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
begin

end;


procedure TForm1.PrewittClick(Sender: TObject);
var
  resultMatrix: RGB_MATRIX;
begin
  if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then
  begin
    ShowMessage('Primero debes cargar una imagen');
    Exit;
  end;

  // Crear matriz para el resultado
  SetLength(resultMatrix, IMG_WIDTH, IMG_HEIGHT, 3);
  
  // Aplicar filtro de Prewitt
  ImageProcessing.EdgeDetectionPrewitt(IMG_HEIGHT, IMG_WIDTH, MATRIX, resultMatrix);
  
  // Actualizar la matriz y la imagen
  MATRIX := resultMatrix;
  ImageProcessing.CopyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, MATRIX, BMAP);
  Image1.Picture.Assign(BMAP);
  
  // Sincronizar matriz HSV
  ImageProcessing.RGBMatrixToHSVMatrix(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_HSV_MATRIX);
end;

procedure TForm1.RestaurarClick(Sender: TObject);
begin
  // Restaurar la imagen a su estado original cargado desde archivo
  if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then
  begin
    ShowMessage('Primero debes cargar una imagen');
    Exit;
  end;

  // Copiar la matriz original a la matriz actual y actualizar la imagen
  MATRIX := ORIGINAL_MATRIX;
  ImageProcessing.CopyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, MATRIX, BMAP);
  Image1.Picture.Assign(BMAP);
  ImageProcessing.RGBMatrixToHSVMatrix(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_HSV_MATRIX);
  COLOR_MODE := 1; // Volver al modo color original (RGB)
  
  // Actualizar StatusBar y MenuItem
  StatusBar1.Panels[0].Text := 'Modo: RGB';
  HSV.Caption := 'Cambiar a HSV';
end;

procedure TForm1.Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  imgX, imgY: Integer;
  r, g, b: Byte;
  h, s, v: Double;
  scaleX, scaleY: Double;
  offsetX, offsetY: Integer;
  displayWidth, displayHeight: Integer;
begin
  // Verificar que haya una imagen cargada
  if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then
    Exit;
  
  // Calcular la escala y offset debido a Stretch y Center
  if Image1.Stretch then
  begin
    if Image1.Proportional then
    begin
      // Calcular aspecto ratio
      scaleX := Image1.Width / IMG_WIDTH;
      scaleY := Image1.Height / IMG_HEIGHT;
      
      // Usar la menor escala para mantener proporciones
      if scaleX < scaleY then
      begin
        displayWidth := Image1.Width;
        displayHeight := Round(IMG_HEIGHT * scaleX);
        offsetX := 0;
        offsetY := (Image1.Height - displayHeight) div 2;
      end
      else
      begin
        displayWidth := Round(IMG_WIDTH * scaleY);
        displayHeight := Image1.Height;
        offsetX := (Image1.Width - displayWidth) div 2;
        offsetY := 0;
      end;
      
      // Ajustar coordenadas del mouse
      X := X - offsetX;
      Y := Y - offsetY;
      
      // Convertir a coordenadas de imagen
      if displayWidth > 0 then
        imgX := Round((X * IMG_WIDTH) / displayWidth)
      else
        Exit;
        
      if displayHeight > 0 then
        imgY := Round((Y * IMG_HEIGHT) / displayHeight)
      else
        Exit;
    end
    else
    begin
      // Sin proporcional, solo stretch
      imgX := Round((X * IMG_WIDTH) / Image1.Width);
      imgY := Round((Y * IMG_HEIGHT) / Image1.Height);
    end;
  end
  else
  begin
    // Sin stretch, las coordenadas son directas (considerar center)
    if Image1.Center then
    begin
      offsetX := (Image1.Width - IMG_WIDTH) div 2;
      offsetY := (Image1.Height - IMG_HEIGHT) div 2;
      imgX := X - offsetX;
      imgY := Y - offsetY;
    end
    else
    begin
      imgX := X;
      imgY := Y;
    end;
  end;
  
  // Verificar que las coordenadas estén dentro de los límites
  if (imgX < 0) or (imgX >= IMG_WIDTH) or (imgY < 0) or (imgY >= IMG_HEIGHT) then
  begin
    StatusBar1.Panels[1].Text := 'Posición: fuera de imagen';
    Exit;
  end;
  
  // Obtener valores RGB del pixel
  r := MATRIX[imgX, imgY, 0];
  g := MATRIX[imgX, imgY, 1];
  b := MATRIX[imgX, imgY, 2];
  
  // Obtener valores HSV del pixel
  h := CONVERTED_HSV_MATRIX[imgX, imgY, 0];
  s := CONVERTED_HSV_MATRIX[imgX, imgY, 1];
  v := CONVERTED_HSV_MATRIX[imgX, imgY, 2];
  
  // Actualizar StatusBar con información del pixel
  StatusBar1.Panels[1].Text := Format('Pos: (%d,%d)', [imgX, imgY]);
  StatusBar1.Panels[2].Text := Format('RGB: (%d,%d,%d)', [r, g, b]);
  StatusBar1.Panels[3].Text := Format('HSV: (%.0f°,%.2f,%.2f)', [h, s, v]);
end;

procedure TForm1.RotacionDerClick(Sender: TObject);
var
  newMatrix: RGB_MATRIX;
  x, y, c: Integer;
  temp: Integer;
begin
  if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then
  begin
    ShowMessage('Primero debes cargar una imagen');
    Exit;
  end;

  // Crear matriz para la imagen rotada (intercambiar dimensiones)
  SetLength(newMatrix, IMG_HEIGHT, IMG_WIDTH, 3);
  
  // Rotar 90° a la derecha (sentido horario)
  for x := 0 to IMG_WIDTH - 1 do
    for y := 0 to IMG_HEIGHT - 1 do
      for c := 0 to 2 do
        newMatrix[IMG_HEIGHT - 1 - y, x, c] := MATRIX[x, y, c];
  
  // Intercambiar dimensiones
  temp := IMG_WIDTH;
  IMG_WIDTH := IMG_HEIGHT;
  IMG_HEIGHT := temp;
  MATRIX := newMatrix;
  
  BMAP.SetSize(IMG_WIDTH, IMG_HEIGHT);
  ImageProcessing.CopyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, MATRIX, BMAP);
  Image1.Picture.Assign(BMAP);
  
  // Actualizar StatusBar
  StatusBar1.Panels[6].Text := IntToStr(IMG_HEIGHT) + 'x' + IntToStr(IMG_WIDTH);
  
  // Sincronizar matriz HSV
  ImageProcessing.RGBMatrixToHSVMatrix(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_HSV_MATRIX);
end;

procedure TForm1.RotacionIzqClick(Sender: TObject);
var
  newMatrix: RGB_MATRIX;
  x, y, c: Integer;
  temp: Integer;
begin
  if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then
  begin
    ShowMessage('Primero debes cargar una imagen');
    Exit;
  end;

  // Crear matriz para la imagen rotada (intercambiar dimensiones)
  SetLength(newMatrix, IMG_HEIGHT, IMG_WIDTH, 3);
  
  // Rotar 90° a la izquierda (sentido antihorario)
  for x := 0 to IMG_WIDTH - 1 do
    for y := 0 to IMG_HEIGHT - 1 do
      for c := 0 to 2 do
        newMatrix[y, IMG_WIDTH - 1 - x, c] := MATRIX[x, y, c];
  
  // Intercambiar dimensiones
  temp := IMG_WIDTH;
  IMG_WIDTH := IMG_HEIGHT;
  IMG_HEIGHT := temp;
  MATRIX := newMatrix;
  
  BMAP.SetSize(IMG_WIDTH, IMG_HEIGHT);
  ImageProcessing.CopyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, MATRIX, BMAP);
  Image1.Picture.Assign(BMAP);
  
  // Actualizar StatusBar
  StatusBar1.Panels[6].Text := IntToStr(IMG_HEIGHT) + 'x' + IntToStr(IMG_WIDTH);
  
  // Sincronizar matriz HSV
  ImageProcessing.RGBMatrixToHSVMatrix(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_HSV_MATRIX);
end;

procedure TForm1.SobelClick(Sender: TObject);
var
  resultMatrix: RGB_MATRIX;
begin
  if (IMG_WIDTH = 0) or (IMG_HEIGHT = 0) then
  begin
    ShowMessage('Primero debes cargar una imagen');
    Exit;
  end;

  // Crear matriz para el resultado
  SetLength(resultMatrix, IMG_WIDTH, IMG_HEIGHT, 3);
  
  // Aplicar filtro de Sobel
  ImageProcessing.EdgeDetectionSobel(IMG_HEIGHT, IMG_WIDTH, MATRIX, resultMatrix);
  
  // Actualizar la matriz y la imagen
  MATRIX := resultMatrix;
  ImageProcessing.CopyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, MATRIX, BMAP);
  Image1.Picture.Assign(BMAP);
  
  // Sincronizar matriz HSV
  ImageProcessing.RGBMatrixToHSVMatrix(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_HSV_MATRIX);
end;

end.
