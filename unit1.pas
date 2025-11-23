unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  ExtCtrls, ExtDlgs, StdCtrls, Math;

type

  //definir tipos propios
   //MATRGB= Array of Array of Array of byte;
   RGB_MATRIX = Array of Array of Array of byte;
   HSV_MATRIX = Array of Array of Array of byte;

  { TForm1 }

  TForm1 = class(TForm)
    Image1: TImage;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    ScrollBox1: TScrollBox;
    StatusBar1: TStatusBar;
    Shape1: TShape;
    procedure FormCreate(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure MenuItem2Click(Sender: TObject);
  private

  public

    //procedure copiaItoM(Al,An: Integer; B: Tbitmap;  var M:RGB_MATRIX);   //copiar de bitmap a matriz con scanline
    //procedure copiaMtoI(Al,An: Integer; M:RGB_MATRIX; var B:Tbitmap  );   //copiar de matriz a la imagen con scanline

    // Procedimiento para copiar una imagen a una matriz
    procedure copyImageToMatrix(imageHeight, imageWidth: Integer; B: TBitmap; var matrix:RGB_MATRIX);

    // Procedimiento para copiar una matriz a una imagen
    procedure copyMatrixToImage(imageHeight, imageWidth: Integer; matrix:RGB_MATRIX; var B:TBitmap);

    // Procedimiento para convertir un valor de RGB a HSV
    procedure RGBToHSVByte(r, g, b: Byte; out Hb, Sb, Vb: Byte);
    procedure RGBMatrixToHSVMatrix(imageHeight, imageWidth: Integer; const RGB: RGB_MATRIX; var HSV: HSV_MATRIX);
  end;

var
  Form1: TForm1;

  HEIGHT, WIDTH: Integer;
  //MAT: RGB_MATRIX ;  //del tipo propio para alamacenar R,G,B
  MATRIX: RGB_MATRIX;
  CONVERTED_HSV_MATRIX:  HSV_MATRIX;

  BMAP: Tbitmap;   //para acceso a imagenes bmp

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.copyImageToMatrix(imageHeight, imageWidth: Integer; B: TBitmap; var matrix:RGB_MATRIX);
var
  i, j, k: Integer;
  P: PByte;
begin
  for i:=0 to imageHeight - 1 do
  begin
    B.BeginUpdate;
    P:= B.ScanLine[i];
    B.EndUpdate;

    for j:=0 to imageWidth - 1 do
    begin
      k:= 3 * j;
      matrix[j, i, 0]:= P[k + 2]; // R
      matrix[j, i, 1]:= P[k + 1]; // G
      matrix[j, i, 2]:= P[k + 0]; // B
    end; // j
  end; // i
end;

procedure TForm1.copyMatrixToImage(imageHeight, imageWidth: Integer; matrix:RGB_MATRIX; var B:TBitmap);
var
  i, j, k: Integer;
  P: PByte;
begin
  for i:=0 to imageHeight - 1 do
  begin
    B.BeginUpdate;
    P:= B.ScanLine[i];
    B.EndUpdate;
    for j:=0 to imageWidth - 1 do
    begin
      k:= 3 * j;
      P[k + 2]:= matrix[j, i, 0]; // R
      P[k + 1]:= matrix[j, i, 1]; // G
      P[k + 0]:= matrix[j, i, 2]; // B
    end; // j
  end; // i

  // HEIGHT := B.Height;
  // WIDTH := B.Width;
end;

procedure TForm1.RGBToHSVByte(r, g, b: Byte; out Hb, Sb, Vb: Byte);
var
  rf, gf, bf, cmax, cmin, delta, H, S, V: Double;
begin
  // 1. Normalizar valores RGB de 0..255 a 0..1
  rf := r / 255.0;
  gf := g / 255.0;
  bf := b / 255.0;

  cmax := Max(rf, Max(gf, bf));
  cmin := Min(rf, Min(gf, bf));
  delta := cmax - cmin;

  // 2. Calcular Valor (V) -> Rango 0..1
  V := cmax;

  // 3. Calcular Saturación (S) -> Rango 0..1
  if cmax = 0 then
    S := 0
  else
    S := delta / cmax;

  // 4. Calcular Matiz (H) -> Rango 0..360 grados
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

    H := H * 60; // Convertir a grados
    if H < 0 then
      H := H + 360;
  end;

  // 5. Convertir todo a Byte (0..255)
  // H se normaliza dividiendo por 360 y multiplicando por 255
  Hb := Round((H / 360) * 255);
  // S y V ya están entre 0 y 1, solo se multiplican por 255
  Sb := Round(S * 255);
  Vb := Round(V * 255);
end;



procedure TForm1.RGBMatrixToHSVMatrix(imageHeight, imageWidth: Integer; const RGB: RGB_MATRIX; var HSV: HSV_MATRIX);

var
  i, j: Integer;
  r, g, b, h, s, v: Byte;
begin
  SetLength(HSV, imageWidth, imageHeight, 3);
  for i := 0 to imageHeight - 1 do
  begin
    for j := 0 to imageWidth - 1 do
    begin
      r := RGB[j, i, 0];
      g := RGB[j, i, 1];
      b := RGB[j, i, 2];
      RGBToHSVByte(r, g, b, h, s, v);
      HSV[j, i, 0] := h;
      HSV[j, i, 1] := s;
      HSV[j, i, 2] := v;
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
   BMAP:=Tbitmap.Create;  //Instanciar-crear objeto de la clase Tbitmap
end;


procedure TForm1.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  
  StatusBar1.Panels[1].Text:=IntToStr(X);
  StatusBar1.Panels[2].Text:=IntToStr(Y);
  StatusBar1.Panels[4].Text:= IntToStr(MATRIX[x,y,0])+','+IntToStr(MATRIX[x,y,1])+','+IntToStr(MATRIX[x,y,2]);
  StatusBar1.Panels[8].Text:= IntToStr(CONVERTED_HSV_MATRIX[x,y,0])+','+IntToStr(CONVERTED_HSV_MATRIX[x,y,1])+','+IntToStr(CONVERTED_HSV_MATRIX[x,y,2]);

  // Mostrar color
  Shape1.Brush.Color := RGBToColor(MATRIX[x, y, 0], MATRIX[x, y, 1], MATRIX[x, y, 2]);
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
begin
    if OpenPictureDialog1.Execute then
    begin
     Image1.Enabled:=True;
     BMAP.LoadFromFile(OpenPictureDialog1.FileName);
     HEIGHT:=BMAP.Height;
     WIDTH:=BMAP.Width;

     if BMAP.PixelFormat<> pf24bit then   //garantizar 8 bits por canal
     begin
      BMAP.PixelFormat:=pf24bit;
     end;
     StatusBar1.Panels[6].Text:=IntToStr(HEIGHT)+'x'+IntToStr(WIDTH);
     SetLength(MATRIX,WIDTH,HEIGHT,3);
     copyImageToMatrix(HEIGHT,WIDTH,BMAP,MATRIX);  //copiar (TPicture)contenido de bitmap a MAT
     Image1.Picture.Assign(BMAP);  //visulaizar imagen
     RGBMatrixToHSVMatrix(HEIGHT, WIDTH, MATRIX, CONVERTED_HSV_MATRIX);
  end;
end;

end.

