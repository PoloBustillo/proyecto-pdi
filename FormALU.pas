unit FormALU;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Math, ImageProcessing;

type

  { TFormALUOperations }

  TFormALUOperations = class(TForm)
    BtnADD: TButton;
    BtnAND: TButton;
    BtnAplicar: TButton;
    BtnCargarImagen2: TButton;
    BtnCerrar: TButton;
    BtnDIV: TButton;
    BtnMUL: TButton;
    BtnNOT: TButton;
    BtnOR: TButton;
    BtnSUB: TButton;
    BtnXOR: TButton;
    ComboModo: TComboBox;
    EditConstante: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    ImageOriginal: TImage;
    ImageResultado: TImage;
    ImageSegunda: TImage;
    Label1: TLabel;
    Label3: TLabel;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    RadioAritmeticas: TRadioButton;
    RadioLogicas: TRadioButton;
    TabImagen1: TTabSheet;
    TabImagen2: TTabSheet;
    TabResultado: TTabSheet;
    TrackConstante: TTrackBar;
    procedure BtnADDClick(Sender: TObject);
    procedure BtnANDClick(Sender: TObject);
    procedure BtnAplicarClick(Sender: TObject);
    procedure BtnCargarImagen2Click(Sender: TObject);
    procedure BtnCerrarClick(Sender: TObject);
    procedure BtnDIVClick(Sender: TObject);
    procedure BtnMULClick(Sender: TObject);
    procedure BtnNOTClick(Sender: TObject);
    procedure BtnORClick(Sender: TObject);
    procedure BtnSUBClick(Sender: TObject);
    procedure BtnXORClick(Sender: TObject);
    procedure ComboModoChange(Sender: TObject);
    procedure RadioAritmeticasClick(Sender: TObject);
    procedure RadioLogicasClick(Sender: TObject);
    procedure TrackConstanteChange(Sender: TObject);
  private
    FSourceMatrix: RGB_MATRIX;
    FSecondMatrix: RGB_MATRIX;
    FImageHeight: Integer;
    FImageWidth: Integer;
    FSecondHeight: Integer;
    FSecondWidth: Integer;
    FHasSecondImage: Boolean;
    FCurrentOperation: string;
    
    procedure ApplyLogicalOperation(operation: string);
    procedure ApplyArithmeticOperation(operation: string);
    procedure ApplyOperationWithConstant(operation: string; constValue: Byte);
    procedure ShowResult(const resultMatrix: RGB_MATRIX);
  public
    procedure SetSourceImage(const srcMatrix: RGB_MATRIX; imgHeight, imgWidth: Integer);
  end;

var
  FormALUOperations: TFormALUOperations;

implementation

{$R *.lfm}

{ TFormALUOperations }

procedure TFormALUOperations.SetSourceImage(const srcMatrix: RGB_MATRIX; 
  imgHeight, imgWidth: Integer);
var
  bmp: TBitmap;
begin
  FImageHeight := imgHeight;
  FImageWidth := imgWidth;
  SetLength(FSourceMatrix, imgWidth, imgHeight, 3);
  FSourceMatrix := Copy(srcMatrix, 0, Length(srcMatrix));
  FHasSecondImage := False;
  
  // Mostrar imagen original
  bmp := TBitmap.Create;
  try
    bmp.PixelFormat := pf24bit;
    bmp.SetSize(imgWidth, imgHeight);
    ImageProcessing.CopyMatrixToImage(imgHeight, imgWidth, FSourceMatrix, bmp);
    ImageOriginal.Picture.Assign(bmp);
  finally
    bmp.Free;
  end;
end;

procedure TFormALUOperations.BtnCargarImagen2Click(Sender: TObject);
var
  bmp: TBitmap;
begin
  if OpenDialog1.Execute then
  begin
    bmp := TBitmap.Create;
    try
      bmp.LoadFromFile(OpenDialog1.FileName);
      FSecondHeight := bmp.Height;
      FSecondWidth := bmp.Width;
      
      // Verificar que las dimensiones coincidan
      if (FSecondWidth <> FImageWidth) or (FSecondHeight <> FImageHeight) then
      begin
        ShowMessage(Format('Las dimensiones no coinciden.'+#13#10+
                          'Imagen 1: %dx%d'+#13#10+
                          'Imagen 2: %dx%d'+#13#10#13#10+
                          'Ambas imágenes deben tener el mismo tamaño.', 
                          [FImageWidth, FImageHeight, FSecondWidth, FSecondHeight]));
        Exit;
      end;
      
      if bmp.PixelFormat <> pf24bit then
        bmp.PixelFormat := pf24bit;
      
      SetLength(FSecondMatrix, FSecondWidth, FSecondHeight, 3);
      ImageProcessing.CopyImageToMatrix(FSecondHeight, FSecondWidth, bmp, FSecondMatrix);
      
      ImageSegunda.Picture.Assign(bmp);
      FHasSecondImage := True;
      
      ShowMessage('Segunda imagen cargada exitosamente');
      PageControl1.ActivePage := TabImagen2;
    finally
      bmp.Free;
    end;
  end;
end;

procedure TFormALUOperations.BtnCerrarClick(Sender: TObject);
begin
  Close;
end;

procedure TFormALUOperations.RadioLogicasClick(Sender: TObject);
begin
  GroupBox2.Visible := True;
  GroupBox3.Visible := False;
end;

procedure TFormALUOperations.RadioAritmeticasClick(Sender: TObject);
begin
  GroupBox2.Visible := False;
  GroupBox3.Visible := True;
end;

procedure TFormALUOperations.ComboModoChange(Sender: TObject);
begin
  if ComboModo.ItemIndex = 0 then // Dos imágenes
  begin
    GroupBox5.Visible := True;
    GroupBox4.Visible := False;
  end
  else // Imagen + Constante
  begin
    GroupBox5.Visible := False;
    GroupBox4.Visible := True;
  end;
end;

procedure TFormALUOperations.TrackConstanteChange(Sender: TObject);
begin
  EditConstante.Text := IntToStr(TrackConstante.Position);
end;

procedure TFormALUOperations.BtnANDClick(Sender: TObject);
begin
  FCurrentOperation := 'AND';
end;

procedure TFormALUOperations.BtnORClick(Sender: TObject);
begin
  FCurrentOperation := 'OR';
end;

procedure TFormALUOperations.BtnXORClick(Sender: TObject);
begin
  FCurrentOperation := 'XOR';
end;

procedure TFormALUOperations.BtnNOTClick(Sender: TObject);
begin
  FCurrentOperation := 'NOT';
end;

procedure TFormALUOperations.BtnADDClick(Sender: TObject);
begin
  FCurrentOperation := 'ADD';
end;

procedure TFormALUOperations.BtnSUBClick(Sender: TObject);
begin
  FCurrentOperation := 'SUB';
end;

procedure TFormALUOperations.BtnMULClick(Sender: TObject);
begin
  FCurrentOperation := 'MUL';
end;

procedure TFormALUOperations.BtnDIVClick(Sender: TObject);
begin
  FCurrentOperation := 'DIV';
end;

procedure TFormALUOperations.BtnAplicarClick(Sender: TObject);
var
  constValue: Integer;
begin
  if (FImageWidth = 0) or (FImageHeight = 0) then
  begin
    ShowMessage('No hay imagen cargada');
    Exit;
  end;
  
  if FCurrentOperation = '' then
  begin
    ShowMessage('Selecciona una operación primero');
    Exit;
  end;
  
  Screen.Cursor := crHourGlass;
  try
    // Modo con dos imágenes
    if ComboModo.ItemIndex = 0 then
    begin
      if not FHasSecondImage and (FCurrentOperation <> 'NOT') then
      begin
        ShowMessage('Debes cargar una segunda imagen para esta operación');
        Exit;
      end;
      
      if RadioLogicas.Checked then
        ApplyLogicalOperation(FCurrentOperation)
      else
        ApplyArithmeticOperation(FCurrentOperation);
    end
    else // Modo con constante
    begin
      constValue := StrToIntDef(EditConstante.Text, 128);
      if (constValue < 0) or (constValue > 255) then
      begin
        ShowMessage('El valor de la constante debe estar entre 0 y 255');
        Exit;
      end;
      
      ApplyOperationWithConstant(FCurrentOperation, Byte(constValue));
    end;
    
    PageControl1.ActivePage := TabResultado;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormALUOperations.ApplyLogicalOperation(operation: string);
var
  x, y, c: Integer;
  resultMatrix: RGB_MATRIX;
begin
  SetLength(resultMatrix, FImageWidth, FImageHeight, 3);
  
  if operation = 'NOT' then
  begin
    // Operación NOT (complemento) - solo necesita una imagen
    for x := 0 to FImageWidth - 1 do
      for y := 0 to FImageHeight - 1 do
        for c := 0 to 2 do
          resultMatrix[x, y, c] := 255 - FSourceMatrix[x, y, c];
  end
  else if operation = 'AND' then
  begin
    // Operación AND (intersección)
    for x := 0 to FImageWidth - 1 do
      for y := 0 to FImageHeight - 1 do
        for c := 0 to 2 do
          resultMatrix[x, y, c] := FSourceMatrix[x, y, c] and FSecondMatrix[x, y, c];
  end
  else if operation = 'OR' then
  begin
    // Operación OR (unión)
    for x := 0 to FImageWidth - 1 do
      for y := 0 to FImageHeight - 1 do
        for c := 0 to 2 do
          resultMatrix[x, y, c] := FSourceMatrix[x, y, c] or FSecondMatrix[x, y, c];
  end
  else if operation = 'XOR' then
  begin
    // Operación XOR (diferencia exclusiva)
    for x := 0 to FImageWidth - 1 do
      for y := 0 to FImageHeight - 1 do
        for c := 0 to 2 do
          resultMatrix[x, y, c] := FSourceMatrix[x, y, c] xor FSecondMatrix[x, y, c];
  end;
  
  ShowResult(resultMatrix);
  SetLength(resultMatrix, 0, 0, 0);
end;

procedure TFormALUOperations.ApplyArithmeticOperation(operation: string);
var
  x, y, c: Integer;
  resultMatrix: RGB_MATRIX;
  tempValue: Integer;
begin
  SetLength(resultMatrix, FImageWidth, FImageHeight, 3);
  
  if operation = 'NOT' then
  begin
    // Para operaciones aritméticas, NOT es complemento a 255
    for x := 0 to FImageWidth - 1 do
      for y := 0 to FImageHeight - 1 do
        for c := 0 to 2 do
          resultMatrix[x, y, c] := 255 - FSourceMatrix[x, y, c];
  end
  else if operation = 'ADD' then
  begin
    // Operación ADD (suma con saturación)
    for x := 0 to FImageWidth - 1 do
      for y := 0 to FImageHeight - 1 do
        for c := 0 to 2 do
        begin
          tempValue := FSourceMatrix[x, y, c] + FSecondMatrix[x, y, c];
          if tempValue > 255 then tempValue := 255;
          resultMatrix[x, y, c] := Byte(tempValue);
        end;
  end
  else if operation = 'SUB' then
  begin
    // Operación SUB (resta con saturación)
    for x := 0 to FImageWidth - 1 do
      for y := 0 to FImageHeight - 1 do
        for c := 0 to 2 do
        begin
          tempValue := FSourceMatrix[x, y, c] - FSecondMatrix[x, y, c];
          if tempValue < 0 then tempValue := 0;
          resultMatrix[x, y, c] := Byte(tempValue);
        end;
  end
  else if operation = 'MUL' then
  begin
    // Operación MUL (multiplicación normalizada)
    for x := 0 to FImageWidth - 1 do
      for y := 0 to FImageHeight - 1 do
        for c := 0 to 2 do
        begin
          tempValue := Round((FSourceMatrix[x, y, c] * FSecondMatrix[x, y, c]) / 255.0);
          if tempValue > 255 then tempValue := 255;
          resultMatrix[x, y, c] := Byte(tempValue);
        end;
  end
  else if operation = 'DIV' then
  begin
    // Operación DIV (división normalizada)
    for x := 0 to FImageWidth - 1 do
      for y := 0 to FImageHeight - 1 do
        for c := 0 to 2 do
        begin
          if FSecondMatrix[x, y, c] > 0 then
            tempValue := Round((FSourceMatrix[x, y, c] * 255.0) / FSecondMatrix[x, y, c])
          else
            tempValue := 255;
          if tempValue > 255 then tempValue := 255;
          resultMatrix[x, y, c] := Byte(tempValue);
        end;
  end;
  
  ShowResult(resultMatrix);
  SetLength(resultMatrix, 0, 0, 0);
end;

procedure TFormALUOperations.ApplyOperationWithConstant(operation: string; constValue: Byte);
var
  x, y, c: Integer;
  resultMatrix: RGB_MATRIX;
  tempValue: Integer;
begin
  SetLength(resultMatrix, FImageWidth, FImageHeight, 3);
  
  if operation = 'NOT' then
  begin
    // NOT no usa constante
    for x := 0 to FImageWidth - 1 do
      for y := 0 to FImageHeight - 1 do
        for c := 0 to 2 do
          resultMatrix[x, y, c] := 255 - FSourceMatrix[x, y, c];
  end
  else if (operation = 'AND') or (operation = 'OR') or (operation = 'XOR') then
  begin
    // Operaciones lógicas con constante
    for x := 0 to FImageWidth - 1 do
      for y := 0 to FImageHeight - 1 do
        for c := 0 to 2 do
        begin
          if operation = 'AND' then
            resultMatrix[x, y, c] := FSourceMatrix[x, y, c] and constValue
          else if operation = 'OR' then
            resultMatrix[x, y, c] := FSourceMatrix[x, y, c] or constValue
          else // XOR
            resultMatrix[x, y, c] := FSourceMatrix[x, y, c] xor constValue;
        end;
  end
  else if operation = 'ADD' then
  begin
    // Suma con constante (saturación)
    for x := 0 to FImageWidth - 1 do
      for y := 0 to FImageHeight - 1 do
        for c := 0 to 2 do
        begin
          tempValue := FSourceMatrix[x, y, c] + constValue;
          if tempValue > 255 then tempValue := 255;
          resultMatrix[x, y, c] := Byte(tempValue);
        end;
  end
  else if operation = 'SUB' then
  begin
    // Resta con constante (saturación)
    for x := 0 to FImageWidth - 1 do
      for y := 0 to FImageHeight - 1 do
        for c := 0 to 2 do
        begin
          tempValue := FSourceMatrix[x, y, c] - constValue;
          if tempValue < 0 then tempValue := 0;
          resultMatrix[x, y, c] := Byte(tempValue);
        end;
  end
  else if operation = 'MUL' then
  begin
    // Multiplicación con constante normalizada
    for x := 0 to FImageWidth - 1 do
      for y := 0 to FImageHeight - 1 do
        for c := 0 to 2 do
        begin
          tempValue := Round((FSourceMatrix[x, y, c] * constValue) / 255.0);
          if tempValue > 255 then tempValue := 255;
          resultMatrix[x, y, c] := Byte(tempValue);
        end;
  end
  else if operation = 'DIV' then
  begin
    // División con constante normalizada
    for x := 0 to FImageWidth - 1 do
      for y := 0 to FImageHeight - 1 do
        for c := 0 to 2 do
        begin
          if constValue > 0 then
            tempValue := Round((FSourceMatrix[x, y, c] * 255.0) / constValue)
          else
            tempValue := 255;
          if tempValue > 255 then tempValue := 255;
          resultMatrix[x, y, c] := Byte(tempValue);
        end;
  end;
  
  ShowResult(resultMatrix);
  SetLength(resultMatrix, 0, 0, 0);
end;

procedure TFormALUOperations.ShowResult(const resultMatrix: RGB_MATRIX);
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  try
    bmp.PixelFormat := pf24bit;
    bmp.SetSize(FImageWidth, FImageHeight);
    ImageProcessing.CopyMatrixToImage(FImageHeight, FImageWidth, resultMatrix, bmp);
    ImageResultado.Picture.Assign(bmp);
  finally
    bmp.Free;
  end;
end;

end.
