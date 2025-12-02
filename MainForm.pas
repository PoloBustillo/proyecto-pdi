unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  ExtCtrls, StdCtrls, ExtDlgs, TAGraph, TASeries, TAChartUtils, Math,
  Constants, ImageProcessing, FormHistogram, FormBinarize, FormGamma, 
  FormFourier, FormALU, UIHelpers;

type

  { TForm1 }

  TForm1 = class(TForm)
    FiltrosColor: TMenuItem;
    Image1: TImage;
    MainMenu1: TMainMenu;
    Abrir: TMenuItem;
    Guardar: TMenuItem;
    Archivo: TMenuItem;
    Gamma: TMenuItem;
    Contraste: TMenuItem;
    HSV: TMenuItem;
    Grises: TMenuItem;
    ContrasteReducir: TMenuItem;
    Espaciales: TMenuItem;
    Fourier: TMenuItem;
    ALU: TMenuItem;
    Textura: TMenuItem;
    Suavizado: TMenuItem;
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
    Binarizacion: TMenuItem;
    Diferencia: TMenuItem;
    Sobel: TMenuItem;
    Prewitt: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    SaveDialog1: TSaveDialog;
    StatusBar1: TStatusBar;

    procedure ALUClick(Sender: TObject);
    procedure ANDOperatorClick(Sender: TObject);
    procedure ContrasteReducirClick(Sender: TObject);
    procedure DiferenciaClick(Sender: TObject);
    procedure DropOpen(Sender: TObject; const FileNames: array of String);
    procedure EscalaMenosClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure AbrirClick(Sender: TObject);
    procedure FourierClick(Sender: TObject);
    procedure GrisesClick(Sender: TObject);
    procedure GuardarClick(Sender: TObject);
    procedure HSVClick(Sender: TObject);
    procedure BinarizacionClick(Sender: TObject);
    procedure GammaClick(Sender: TObject);
    procedure HistogramaClick(Sender: TObject);
    procedure ContrasteClick(Sender: TObject);
    procedure EscalaMasClick(Sender: TObject);
    procedure SuavizadoClick(Sender: TObject);
    procedure PrewittClick(Sender: TObject);
    procedure RestaurarClick(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure RotacionDerClick(Sender: TObject);
    procedure RotacionIzqClick(Sender: TObject);
    procedure SobelClick(Sender: TObject);
    procedure TexturaClick(Sender: TObject);
  end;

var
  Form1: TForm1;
  // Variables globales de imagen
  ALTO_IMG, ANCHO_IMG, MODO_COLOR: Integer;
  MATRIZ, MATRIZ_RESPALDO: RGB_MATRIX;
  MATRIZ_HSV: HSV_MATRIX;
  MATRIZ_GRISES: RGB_MATRIX;
  MAPA_BITS: TBitmap;

implementation

{$R *.lfm}

{ TForm1 }

// Función auxiliar para verificar si hay una imagen cargada
function ImagenCargada: Boolean;
begin
  Result := (ANCHO_IMG > 0) and (ALTO_IMG > 0);
  if not Result then
    ShowMessage('Primero debes cargar una imagen');
end;

// Función auxiliar para crear matriz de resultado con dimensiones actuales
function CrearMatrizResultado: RGB_MATRIX;
begin
  SetLength(Result, ANCHO_IMG, ALTO_IMG, 3);
end;

// Procedimiento auxiliar para actualizar y mostrar la matriz procesada (con nueva matriz)
procedure ActualizarImagen(const nuevaMatriz: RGB_MATRIX);
begin
  MATRIZ := nuevaMatriz;
  ImageProcessing.CopiarMatrizAImagen(ALTO_IMG, ANCHO_IMG, MATRIZ, MAPA_BITS);
  Form1.Image1.Picture.Assign(MAPA_BITS);
  // Sincronizar matriz HSV
  ImageProcessing.MatrizRGBaMatrizHSV(ALTO_IMG, ANCHO_IMG, MATRIZ, MATRIZ_HSV);
end;

// Procedimiento auxiliar para mostrar MATRIZ actual (sin asignar nueva matriz)
procedure MostrarImagen;
begin
  ImageProcessing.CopiarMatrizAImagen(ALTO_IMG, ANCHO_IMG, MATRIZ, MAPA_BITS);
  Form1.Image1.Picture.Assign(MAPA_BITS);
  // Sincronizar matriz HSV
  ImageProcessing.MatrizRGBaMatrizHSV(ALTO_IMG, ANCHO_IMG, MATRIZ, MATRIZ_HSV);
end;

// Procedimiento auxiliar para operaciones que cambian dimensiones (escala, rotación)
procedure ActualizarImagenConDimensiones(const nuevaMatriz: RGB_MATRIX; nuevoAncho, nuevoAlto: Integer);
begin
  ANCHO_IMG := nuevoAncho;
  ALTO_IMG := nuevoAlto;
  MATRIZ := nuevaMatriz;
  
  MAPA_BITS.SetSize(ANCHO_IMG, ALTO_IMG);
  ImageProcessing.CopiarMatrizAImagen(ALTO_IMG, ANCHO_IMG, MATRIZ, MAPA_BITS);
  Form1.Image1.Picture.Assign(MAPA_BITS);
  
  // Actualizar StatusBar
  Form1.StatusBar1.Panels[6].Text := IntToStr(ALTO_IMG) + 'x' + IntToStr(ANCHO_IMG);
  
  // Sincronizar matriz HSV
  ImageProcessing.MatrizRGBaMatrizHSV(ALTO_IMG, ANCHO_IMG, MATRIZ, MATRIZ_HSV);
end;

// Procedimiento auxiliar para cargar una imagen desde archivo
procedure CargarImagenDesdeArchivo(const nombreArchivo: string);
begin
  // Limpiar matrices anteriores
  SetLength(MATRIZ, 0, 0, 0);
  SetLength(MATRIZ_RESPALDO, 0, 0, 0);
  SetLength(MATRIZ_GRISES, 0, 0, 0);
  SetLength(MATRIZ_HSV, 0, 0, 0);
  
  // Cargar la imagen
  Form1.Image1.Enabled := True;
  MAPA_BITS.LoadFromFile(nombreArchivo);
  ALTO_IMG := MAPA_BITS.Height;
  ANCHO_IMG := MAPA_BITS.Width;
  
  // Asegurar formato de 24 bits
  if MAPA_BITS.PixelFormat <> pf24bit then
    MAPA_BITS.PixelFormat := pf24bit;
  
  // Actualizar StatusBar con dimensiones
  Form1.StatusBar1.Panels[6].Text := IntToStr(ALTO_IMG) + 'x' + IntToStr(ANCHO_IMG);
  
  // Crear matrices para la imagen
  SetLength(MATRIZ, ANCHO_IMG, ALTO_IMG, 3);
  SetLength(MATRIZ_RESPALDO, ANCHO_IMG, ALTO_IMG, 3);
  
  // Copiar imagen a las matrices
  ImageProcessing.CopiarImagenAMatriz(ALTO_IMG, ANCHO_IMG, MAPA_BITS, MATRIZ);
  ImageProcessing.CopiarImagenAMatriz(ALTO_IMG, ANCHO_IMG, MAPA_BITS, MATRIZ_RESPALDO);
  
  // Mostrar imagen
  Form1.Image1.Picture.Assign(MAPA_BITS);
  
  // Inicializar matriz HSV
  ImageProcessing.MatrizRGBaMatrizHSV(ALTO_IMG, ANCHO_IMG, MATRIZ, MATRIZ_HSV);
  
  // Configurar modo de color inicial
  MODO_COLOR := 1;
  Form1.StatusBar1.Panels[0].Text := 'Modo: RGB';
  Form1.HSV.Caption := 'Cambiar a HSV';
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  MAPA_BITS := TBitmap.Create;

  // Inicializar StatusBar
  if StatusBar1.Panels.Count > 0 then
    StatusBar1.Panels[0].Text := 'Modo: RGB';
end;

procedure TForm1.DiferenciaClick(Sender: TObject);
var
  resultMatrix: RGB_MATRIX;
begin
  if not ImagenCargada then Exit;

  resultMatrix := CrearMatrizResultado;
  
  // Aplicar detección de bordes por diferencia
  ImageProcessing.DeteccionBordesDiferencia(ALTO_IMG, ANCHO_IMG, MATRIZ, resultMatrix);
  ActualizarImagen(resultMatrix);
end;

procedure TForm1.DropOpen(Sender: TObject; const FileNames: array of String);
var
  extension: string;
begin
  // Verificar que al menos un archivo fue arrastrado
  if Length(FileNames) = 0 then
    Exit;
  
  // Verificar que el archivo existe
  if not FileExists(FileNames[0]) then
  begin
    ShowMessage('El archivo no existe: ' + FileNames[0]);
    Exit;
  end;
  
  // Verificar que sea un archivo de imagen válido
  extension := LowerCase(ExtractFileExt(FileNames[0]));
  if not ((extension = '.bmp') or (extension = '.jpg') or 
          (extension = '.jpeg') or (extension = '.png')) then
  begin
    ShowMessage('Formato no soportado. Solo se aceptan archivos BMP, JPG, JPEG y PNG.');
    Exit;
  end;
  
  try
    CargarImagenDesdeArchivo(FileNames[0]);
    
    // Guardar nombre del archivo en OpenDialog para usar en GuardarClick
    OpenDialog1.FileName := FileNames[0];
    
  except
    on E: Exception do
      ShowMessage('Error al cargar la imagen: ' + E.Message);
  end;
end;

procedure TForm1.EscalaMenosClick(Sender: TObject);
var
  newWidth, newHeight: Integer;
  resultMatrix: RGB_MATRIX;
begin
  if not ImagenCargada then Exit;

  // Verificar tamaño mínimo
  if (ANCHO_IMG div 2 < 10) or (ALTO_IMG div 2 < 10) then
  begin
    ShowMessage('La imagen es demasiado pequeña para reducir más');
    Exit;
  end;

  // Aplicar reducción de escala con interpolación bilineal
  ImageProcessing.EscalarAbajoBilineal(ALTO_IMG, ANCHO_IMG, MATRIZ, resultMatrix, newWidth, newHeight);
  ActualizarImagenConDimensiones(resultMatrix, newWidth, newHeight);
end;


procedure TForm1.ContrasteReducirClick(Sender: TObject);
var
  x, y, c: Integer;
  value: Byte;
  newValue: Integer;
begin
  if not ImagenCargada then Exit;

  // Reducir contraste: comprimir valores al rango [64, 192]
  for x := 0 to ANCHO_IMG - 1 do
    for y := 0 to ALTO_IMG - 1 do
      for c := 0 to 2 do
      begin
        value := MATRIZ[x, y, c];
        // Mapear [0,255] → [64,192]
        newValue := 64 + Round((value * 128) / 255);
        MATRIZ[x, y, c] := Byte(newValue);
      end;

  MostrarImagen;
end;

procedure TForm1.ANDOperatorClick(Sender: TObject);
begin

end;

procedure TForm1.ALUClick(Sender: TObject);
var
  ALUForm: TFormALUOperations;
begin
  if not ImagenCargada then Exit;

  // Crear y mostrar el formulario de operaciones ALU
  ALUForm := TFormALUOperations.Create(Self);
  try
    // En modo HSV, convertir a RGB temporalmente para el formulario
    if MODO_COLOR = 3 then
    begin
      ImageProcessing.MatrizHSVaMatrizRGB(ALTO_IMG, ANCHO_IMG, MATRIZ_HSV, MATRIZ);
      ALUForm.SetSourceImage(MATRIZ, ALTO_IMG, ANCHO_IMG);
    end
    else
    begin
      // Pasar la imagen actual al formulario (ya en RGB)
      ALUForm.SetSourceImage(MATRIZ, ALTO_IMG, ANCHO_IMG);
    end;

    // Mostrar el formulario de manera modal
    ALUForm.ShowModal;
  finally
    ALUForm.Free;
  end;
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
    CargarImagenDesdeArchivo(OpenDialog1.FileName);
end;

procedure TForm1.FourierClick(Sender: TObject);
var
  FourierForm: TFormFourierTransform;
begin
  if not ImagenCargada then Exit;

  // Crear y mostrar el formulario de Transformada de Fourier
  FourierForm := TFormFourierTransform.Create(Self);
  try
    // En modo HSV, convertir a RGB temporalmente para el formulario
    if MODO_COLOR = 3 then
    begin
      ImageProcessing.MatrizHSVaMatrizRGB(ALTO_IMG, ANCHO_IMG, MATRIZ_HSV, MATRIZ);
      FourierForm.SetSourceImage(MATRIZ, ALTO_IMG, ANCHO_IMG);
    end
    else
    begin
      // Pasar la imagen actual al formulario (ya en RGB)
      FourierForm.SetSourceImage(MATRIZ, ALTO_IMG, ANCHO_IMG);
    end;

    // Mostrar el formulario de manera modal
    FourierForm.ShowModal;
  finally
    FourierForm.Free;
  end;
end;

procedure TForm1.GrisesClick(Sender: TObject);
begin
  if not ImagenCargada then Exit;

  // Convertir a escala de grises según el modo actual
  if MODO_COLOR = 3 then  // Modo HSV
  begin
    // Aplicar escala de grises en HSV
    ImageProcessing.HSVaEscalaGrises(ALTO_IMG, ANCHO_IMG, MATRIZ_HSV);
    
    // Convertir HSV a RGB en MATRIZ para visualización en monitor
    ImageProcessing.MatrizHSVaMatrizRGB(ALTO_IMG, ANCHO_IMG, MATRIZ_HSV, MATRIZ);
    ImageProcessing.CopiarMatrizAImagen(ALTO_IMG, ANCHO_IMG, MATRIZ, MAPA_BITS);
    Image1.Picture.Assign(MAPA_BITS);
    
    ShowMessage('Escala de grises aplicada en modo HSV (S=0, preservando V)');
  end
  else  // Modo RGB
  begin
    // Usar función del módulo ImageProcessing para RGB
    ImageProcessing.EscalaGrisesRangoMedio(ALTO_IMG, ANCHO_IMG, MATRIZ, MATRIZ_GRISES);
    ImageProcessing.CopiarMatrizAImagen(ALTO_IMG, ANCHO_IMG, MATRIZ_GRISES, MAPA_BITS);
    Image1.Picture.Assign(MAPA_BITS);
    MODO_COLOR := 2;
  end;
end;

procedure TForm1.GuardarClick(Sender: TObject);
var
  extension: string;
begin
  // Verificar que haya una imagen cargada
  if (ANCHO_IMG = 0) or (ALTO_IMG = 0) then
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
      if MODO_COLOR = 3 then
      begin
        // En modo HSV: convertir MATRIZ_HSV a MATRIZ (RGB) para guardar
        ImageProcessing.MatrizHSVaMatrizRGB(ALTO_IMG, ANCHO_IMG, MATRIZ_HSV, MATRIZ);
      end;
      
      // Actualizar MAPA_BITS con MATRIZ (siempre RGB en este punto)
      ImageProcessing.CopiarMatrizAImagen(ALTO_IMG, ANCHO_IMG, MATRIZ, MAPA_BITS);
      
      // Obtener la extensión del archivo
      extension := LowerCase(ExtractFileExt(SaveDialog1.FileName));
      
      // Guardar según el formato seleccionado
      if (extension = '.bmp') then
      begin
        MAPA_BITS.SaveToFile(SaveDialog1.FileName);
      end
      else if (extension = '.jpg') or (extension = '.jpeg') then
      begin
        // Para JPEG, crear un objeto temporal
        with TJPEGImage.Create do
        try
          Assign(MAPA_BITS);
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
          Assign(MAPA_BITS);
          SaveToFile(SaveDialog1.FileName);
        finally
          Free;
        end;
      end
      else
      begin
        // Por defecto, guardar como BMP
        MAPA_BITS.SaveToFile(SaveDialog1.FileName);
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
  if not ImagenCargada then Exit;

  if MODO_COLOR = 3 then
  begin
    // Convertir HSV de vuelta a RGB (MATRIZ_HSV → MATRIZ)
    ImageProcessing.MatrizHSVaMatrizRGB(ALTO_IMG, ANCHO_IMG, MATRIZ_HSV, MATRIZ);
    ImageProcessing.CopiarMatrizAImagen(ALTO_IMG, ANCHO_IMG, MATRIZ, MAPA_BITS);
    Image1.Picture.Assign(MAPA_BITS);
    MODO_COLOR := 1;
    StatusBar1.Panels[0].Text := 'Modo: RGB';
    HSV.Caption := 'Cambiar a HSV';
    ShowMessage('Imagen convertida a modo RGB');
  end
  else
  begin
    // Convertir RGB a HSV (MATRIZ → MATRIZ_HSV)
    ImageProcessing.MatrizRGBaMatrizHSV(ALTO_IMG, ANCHO_IMG, MATRIZ, MATRIZ_HSV);
    
    // Actualizar MATRIZ con la representación RGB para display (monitor requiere RGB)
    ImageProcessing.MatrizHSVaMatrizRGB(ALTO_IMG, ANCHO_IMG, MATRIZ_HSV, MATRIZ);
    ImageProcessing.CopiarMatrizAImagen(ALTO_IMG, ANCHO_IMG, MATRIZ, MAPA_BITS);
    Image1.Picture.Assign(MAPA_BITS);
    
    MODO_COLOR := 3;
    StatusBar1.Panels[0].Text := 'Modo: HSV';
    HSV.Caption := 'Cambiar a RGB';
    ShowMessage('Imagen convertida a modo HSV. Los filtros ahora operan en espacio HSV.');
  end;
end;

procedure TForm1.BinarizacionClick(Sender: TObject);
var
  BinarizeForm: TFormBinarization;
begin
  if not ImagenCargada then Exit;

  // Crear y mostrar el formulario de binarización
  BinarizeForm := TFormBinarization.Create(Self);
  try
    // En modo HSV, convertir a RGB temporalmente para el formulario
    if MODO_COLOR = 3 then
    begin
      ImageProcessing.MatrizHSVaMatrizRGB(ALTO_IMG, ANCHO_IMG, MATRIZ_HSV, MATRIZ);
      BinarizeForm.SetSourceImage(MATRIZ, ALTO_IMG, ANCHO_IMG);
    end
    else
    begin
      // Pasar la imagen actual al formulario (ya en RGB)
      BinarizeForm.SetSourceImage(MATRIZ, ALTO_IMG, ANCHO_IMG);
    end;

    // Si el usuario hace clic en "Aplicar" (mrOk), actualizar la matriz
    if BinarizeForm.ShowModal = mrOk then
    begin
      // Obtener el resultado en MATRIZ (siempre RGB del formulario)
      MATRIZ := BinarizeForm.GetResultMatrix;
      
      if MODO_COLOR = 3 then
      begin
        // En modo HSV, convertir resultado RGB de vuelta a HSV
        ImageProcessing.MatrizRGBaMatrizHSV(ALTO_IMG, ANCHO_IMG, MATRIZ, MATRIZ_HSV);
        // Actualizar MATRIZ con representación RGB para visualización
        ImageProcessing.MatrizHSVaMatrizRGB(ALTO_IMG, ANCHO_IMG, MATRIZ_HSV, MATRIZ);
      end
      else
      begin
        // En modo RGB, sincronizar MATRIZ_HSV
        ImageProcessing.MatrizRGBaMatrizHSV(ALTO_IMG, ANCHO_IMG, MATRIZ, MATRIZ_HSV);
      end;
      
      ImageProcessing.CopiarMatrizAImagen(ALTO_IMG, ANCHO_IMG, MATRIZ, MAPA_BITS);
      Image1.Picture.Assign(MAPA_BITS);
    end;
  finally
    BinarizeForm.Free;
  end;
end;

procedure TForm1.GammaClick(Sender: TObject);
var
  GammaForm: TFormGammaCorrection;
begin
  if not ImagenCargada then Exit;

  // Crear y mostrar el formulario de corrección gamma
  GammaForm := TFormGammaCorrection.Create(Self);
  try
    // Preparar imagen RGB para el formulario
    if MODO_COLOR = 3 then
    begin
      // En modo HSV: convertir a RGB para el formulario
      ImageProcessing.MatrizHSVaMatrizRGB(ALTO_IMG, ANCHO_IMG, MATRIZ_HSV, MATRIZ);
      GammaForm.SetSourceImage(MATRIZ, ALTO_IMG, ANCHO_IMG);
      
      ShowMessage('En modo HSV, la corrección Gamma se aplicará al canal V (Brillo) solamente, ' +
                  'preservando H (Matiz) y S (Saturación).');
    end
    else
    begin
      // Pasar la imagen actual al formulario (ya en RGB)
      GammaForm.SetSourceImage(MATRIZ, ALTO_IMG, ANCHO_IMG);
    end;

    // Si el usuario hace clic en "Aplicar" (mrOk), actualizar la matriz
    if GammaForm.ShowModal = mrOk then
    begin
      // Obtener resultado en MATRIX
      MATRIZ := GammaForm.GetResultMatrix;
      
      if MODO_COLOR = 3 then
      begin
        // En modo HSV: aplicar gamma solo al canal V, preservando H y S
        ImageProcessing.AplicarGammaAValorHSV(ALTO_IMG, ANCHO_IMG, MATRIZ, MATRIZ_HSV);
        
        // Actualizar MATRIZ con representación RGB para visualización
        ImageProcessing.MatrizHSVaMatrizRGB(ALTO_IMG, ANCHO_IMG, MATRIZ_HSV, MATRIZ);
      end
      else
      begin
        // En modo RGB: sincronizar MATRIZ_HSV
        ImageProcessing.MatrizRGBaMatrizHSV(ALTO_IMG, ANCHO_IMG, MATRIZ, MATRIZ_HSV);
      end;
      
      ImageProcessing.CopiarMatrizAImagen(ALTO_IMG, ANCHO_IMG, MATRIZ, MAPA_BITS);
      Image1.Picture.Assign(MAPA_BITS);
    end;
  finally
    GammaForm.Free;
  end;
end;

// Procedimiento auxiliar para calcular y mostrar el histograma en un formulario aparte
procedure ShowImageHistogram;
begin
  UIHelpers.ShowImageHistogram(ANCHO_IMG, ALTO_IMG, MATRIZ, MATRIZ_HSV, MODO_COLOR);
end;

procedure TForm1.HistogramaClick(Sender: TObject);
begin
  ShowImageHistogram;
end;

procedure TForm1.ContrasteClick(Sender: TObject);
begin
  if not ImagenCargada then Exit;

  // Aplicar contraste automático según el modo actual
  if MODO_COLOR = 3 then  // Modo HSV
  begin
    // Aplicar contraste automático solo al canal V (brillo)
    ImageProcessing.ContrasteAutomaticoHSV(ALTO_IMG, ANCHO_IMG, MATRIZ_HSV);
    
    // Convertir HSV a RGB para visualización
    ImageProcessing.MatrizHSVaMatrizRGB(ALTO_IMG, ANCHO_IMG, MATRIZ_HSV, MATRIZ);
    ImageProcessing.CopiarMatrizAImagen(ALTO_IMG, ANCHO_IMG, MATRIZ, MAPA_BITS);
    Image1.Picture.Assign(MAPA_BITS);
  end
  else  // Modo RGB
  begin
    // Aplicar contraste automático a cada canal RGB independientemente
    ImageProcessing.ContrasteAutomatico(ALTO_IMG, ANCHO_IMG, MATRIZ);
    ImageProcessing.CopiarMatrizAImagen(ALTO_IMG, ANCHO_IMG, MATRIZ, MAPA_BITS);
    Image1.Picture.Assign(MAPA_BITS);
    
    // Sincronizar matriz HSV
    ImageProcessing.MatrizRGBaMatrizHSV(ALTO_IMG, ANCHO_IMG, MATRIZ, MATRIZ_HSV);
  end;
end;

procedure TForm1.EscalaMasClick(Sender: TObject);
var
  newWidth, newHeight: Integer;
  resultMatrix: RGB_MATRIX;
begin
  if not ImagenCargada then Exit;

  // Verificar tamaño máximo
  if (ANCHO_IMG * 2 > 20000) or (ALTO_IMG * 2 > 20000) then
  begin
    ShowMessage('La imagen resultante sería demasiado grande (límite: 20000x20000)');
    Exit;
  end;

  // Aplicar aumento de escala con interpolación bilineal
  ImageProcessing.EscalarArribaBilineal(ALTO_IMG, ANCHO_IMG, MATRIZ, resultMatrix, newWidth, newHeight);
  ActualizarImagenConDimensiones(resultMatrix, newWidth, newHeight);
end;

procedure TForm1.SuavizadoClick(Sender: TObject);
var
  resultMatrix: RGB_MATRIX;
  maskSizeStr, trimAmountStr: string;
  maskSize, trimAmount: Integer;
begin
  if not ImagenCargada then Exit;

  // Solicitar parámetros al usuario
  maskSizeStr := '5';
  if not InputQuery('Suavizado Recortado', 'Tamaño de máscara (t×t, impar):', maskSizeStr) then
    Exit;
  
  maskSize := StrToIntDef(maskSizeStr, 5);
  if (maskSize < 3) or (maskSize > 15) or (maskSize mod 2 = 0) then
  begin
    ShowMessage('El tamaño debe ser impar entre 3 y 15');
    Exit;
  end;
  
  trimAmountStr := '2';
  if not InputQuery('Suavizado Recortado', 'Cantidad a descartar de cada extremo (a):', trimAmountStr) then
    Exit;
    
  trimAmount := StrToIntDef(trimAmountStr, 2);
  if (trimAmount < 0) or (trimAmount >= maskSize * maskSize div 4) then
  begin
    ShowMessage('Valor inválido. Debe ser menor que ' + IntToStr(maskSize * maskSize div 4));
    Exit;
  end;

  resultMatrix := CrearMatrizResultado;
  
  // Aplicar suavizado recortado
  ImageProcessing.SuavizadoRecortado(ALTO_IMG, ANCHO_IMG, MATRIZ, resultMatrix, maskSize, trimAmount);
  ActualizarImagen(resultMatrix);
  
  ShowMessage(Format('Suavizado aplicado: máscara %d×%d, descartando %d valores extremos', [maskSize, maskSize, trimAmount]));
end;


procedure TForm1.PrewittClick(Sender: TObject);
var
  resultMatrix: RGB_MATRIX;
begin
  if not ImagenCargada then Exit;

  resultMatrix := CrearMatrizResultado;
  
  // Aplicar filtro de Prewitt
  ImageProcessing.DeteccionBordesPrewitt(ALTO_IMG, ANCHO_IMG, MATRIZ, resultMatrix);
  ActualizarImagen(resultMatrix);
end;

procedure TForm1.RestaurarClick(Sender: TObject);
begin
  // Restaurar la imagen a su estado original cargado desde archivo
  if not ImagenCargada then Exit;

  // Copiar la matriz original a la matriz actual y actualizar la imagen
  ActualizarImagen(MATRIZ_RESPALDO);
  MODO_COLOR := 1; // Volver al modo color original (RGB)
  
  // Actualizar StatusBar y MenuItem
  StatusBar1.Panels[0].Text := 'Modo: RGB';
  HSV.Caption := 'Cambiar a HSV';
end;

procedure TForm1.Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  imgX, imgY: Integer;
  isValid: Boolean;
  r, g, b: Byte;
  h, s, v: Double;
begin
  // Calcular coordenadas de imagen desde coordenadas del mouse
  UIHelpers.CalculateImageCoordinates(Image1, ANCHO_IMG, ALTO_IMG, X, Y,
    Image1.Stretch, Image1.Proportional, Image1.Center, imgX, imgY, isValid);
  
  if not isValid then
  begin
    StatusBar1.Panels[1].Text := 'Posición: fuera de imagen';
    Exit;
  end;
  
  // Obtener valores RGB del pixel
  r := MATRIZ[imgX, imgY, 0];
  g := MATRIZ[imgX, imgY, 1];
  b := MATRIZ[imgX, imgY, 2];
  
  // Obtener valores HSV del pixel
  h := MATRIZ_HSV[imgX, imgY, 0];
  s := MATRIZ_HSV[imgX, imgY, 1];
  v := MATRIZ_HSV[imgX, imgY, 2];
  
  // Actualizar StatusBar con información del pixel
  StatusBar1.Panels[1].Text := Format('Pos: (%d,%d)', [imgX, imgY]);
  StatusBar1.Panels[2].Text := Format('RGB: (%d,%d,%d)', [r, g, b]);
  StatusBar1.Panels[3].Text := Format('HSV: (%.0f°,%.2f,%.2f)', [h, s, v]);
end;

procedure TForm1.RotacionDerClick(Sender: TObject);
var
  newWidth, newHeight: Integer;
  resultMatrix: RGB_MATRIX;
begin
  if not ImagenCargada then Exit;

  // Aplicar rotación 90° a la derecha
  ImageProcessing.RotarDerecha90(ALTO_IMG, ANCHO_IMG, MATRIZ, resultMatrix, newWidth, newHeight);
  ActualizarImagenConDimensiones(resultMatrix, newWidth, newHeight);
end;

procedure TForm1.RotacionIzqClick(Sender: TObject);
var
  newWidth, newHeight: Integer;
  resultMatrix: RGB_MATRIX;
begin
  if not ImagenCargada then Exit;

  // Aplicar rotación 90° a la izquierda
  ImageProcessing.RotarIzquierda90(ALTO_IMG, ANCHO_IMG, MATRIZ, resultMatrix, newWidth, newHeight);
  ActualizarImagenConDimensiones(resultMatrix, newWidth, newHeight);
end;

procedure TForm1.SobelClick(Sender: TObject);
var
  resultMatrix: RGB_MATRIX;
begin
  if not ImagenCargada then Exit;

  resultMatrix := CrearMatrizResultado;
  
  // Aplicar filtro de Sobel
  ImageProcessing.DeteccionBordesSobel(ALTO_IMG, ANCHO_IMG, MATRIZ, resultMatrix);
  ActualizarImagen(resultMatrix);
end;

procedure TForm1.TexturaClick(Sender: TObject);
var
  resultMatrix: RGB_MATRIX;
begin
  if not ImagenCargada then Exit;

  resultMatrix := CrearMatrizResultado;
  
  // Aplicar codificación de textura (Local Binary Pattern)
  ImageProcessing.TexturaCodificada(ALTO_IMG, ANCHO_IMG, MATRIZ, resultMatrix);
  ActualizarImagen(resultMatrix);
  
  ShowMessage('Textura codificada aplicada (Local Binary Pattern en regiones 3×3)');
end;


end.
