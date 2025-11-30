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
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    Binarizacion: TMenuItem;
    MenuItem8: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    StatusBar1: TStatusBar;

    procedure FormCreate(Sender: TObject);
    procedure AbrirClick(Sender: TObject);
    procedure GrisesClick(Sender: TObject);
    procedure BinarizacionClick(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
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

procedure TForm1.MenuItem10Click(Sender: TObject);
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

end.
