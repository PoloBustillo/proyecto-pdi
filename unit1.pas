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
    Grises: TMenuItem;
    Guardar: TMenuItem;
    MenuItem1: TMenuItem;
    Gamma: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    Histograma: TMenuItem;
    HSV: TMenuItem;
    MenuItem5: TMenuItem;
    Binarizacion: TMenuItem;
    Restaurar: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;

    procedure FormCreate(Sender: TObject);
    procedure AbrirClick(Sender: TObject);
    procedure GrisesClick(Sender: TObject);
    procedure GuardarClick(Sender: TObject);
    procedure BinarizacionClick(Sender: TObject);
    procedure GammaClick(Sender: TObject);
    procedure HistogramaClick(Sender: TObject);
    procedure MenuItem11Click(Sender: TObject);
    procedure RestaurarClick(Sender: TObject);
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
  end;
end;

procedure TForm1.GrisesClick(Sender: TObject);
begin
  // Usar función del módulo ImageProcessing
  ImageProcessing.MediumRangeGrayScale(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_GRAY_MATRIX);
  ImageProcessing.CopyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, CONVERTED_GRAY_MATRIX, BMAP);
  Image1.Picture.Assign(BMAP);
  COLOR_MODE := 2;
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
      // Actualizar BMAP con la matriz actual antes de guardar
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
    // Pasar la imagen actual al formulario
    BinarizeForm.SetSourceImage(MATRIX, IMG_HEIGHT, IMG_WIDTH);

    // Si el usuario hace clic en "Aplicar" (mrOk), actualizar la matriz
    if BinarizeForm.ShowModal = mrOk then
    begin
      // Obtener el resultado directamente
      MATRIX := BinarizeForm.GetResultMatrix;

      // Actualizar la imagen en el formulario principal
      ImageProcessing.CopyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, MATRIX, BMAP);
      Image1.Picture.Assign(BMAP);
      ImageProcessing.RGBMatrixToHSVMatrix(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_HSV_MATRIX);
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
    // Pasar la imagen actual al formulario
    GammaForm.SetSourceImage(MATRIX, IMG_HEIGHT, IMG_WIDTH);

    // Si el usuario hace clic en "Aplicar" (mrOk), actualizar la matriz
    if GammaForm.ShowModal = mrOk then
    begin
      // Obtener el resultado directamente
      MATRIX := GammaForm.GetResultMatrix;

      // Actualizar la imagen en el formulario principal
      ImageProcessing.CopyMatrixToImage(IMG_HEIGHT, IMG_WIDTH, MATRIX, BMAP);
      Image1.Picture.Assign(BMAP);
      ImageProcessing.RGBMatrixToHSVMatrix(IMG_HEIGHT, IMG_WIDTH, MATRIX, CONVERTED_HSV_MATRIX);
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

  // Recorrer la imagen y acumular frecuencias
  for x := 0 to IMG_WIDTH - 1 do
    for y := 0 to IMG_HEIGHT - 1 do
    begin
      r := MATRIX[x, y, 0];
      g := MATRIX[x, y, 1];
      b := MATRIX[x, y, 2];

      // Acumular frecuencias por canal
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

  // Mostrar el histograma en el formulario dedicado
  FormHist.ShowHistogram(histData);
end;

procedure TForm1.HistogramaClick(Sender: TObject);
begin
  ShowImageHistogram;
end;

procedure TForm1.MenuItem11Click(Sender: TObject);
begin

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
  COLOR_MODE := 1; // Volver al modo color original
end;

end.
