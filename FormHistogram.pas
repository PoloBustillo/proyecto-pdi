unit FormHistogram;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, TAGraph, TASeries,
  TAChartUtils, ImageProcessing;

type

  { TFormHist }

  TFormHist = class(TForm)
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    Chart1LineSeries2: TLineSeries;
    Chart1LineSeries3: TLineSeries;
    Chart1LineSeries4: TLineSeries;
    procedure FormCreate(Sender: TObject);
  private

  public
    procedure ShowHistogram(const histData: THistogramData; colorMode: Integer = 0);
  end;

var
  FormHist: TFormHist;

implementation

{$R *.lfm}

{ TFormHist }

procedure TFormHist.FormCreate(Sender: TObject);
begin
  // Configurar el gráfico
  Self.Caption := 'Histograma de la Imagen';
  
  // Configurar series - Intensidad (gris)
  Chart1LineSeries1.Title := 'Intensidad';
  Chart1LineSeries1.SeriesColor := clGray;
  Chart1LineSeries1.LinePen.Width := 2;
  
  // Configurar series - Rojo
  Chart1LineSeries2.Title := 'Rojo';
  Chart1LineSeries2.SeriesColor := clRed;
  Chart1LineSeries2.LinePen.Width := 2;
  
  // Configurar series - Verde
  Chart1LineSeries3.Title := 'Verde';
  Chart1LineSeries3.SeriesColor := clLime;
  Chart1LineSeries3.LinePen.Width := 2;
  
  // Configurar series - Azul
  Chart1LineSeries4.Title := 'Azul';
  Chart1LineSeries4.SeriesColor := clBlue;
  Chart1LineSeries4.LinePen.Width := 2;
  
  // Configurar ejes
  try
    // Eje X (valores de 0 a 255)
    Chart1.AxisList[1].Range.UseMin := True;
    Chart1.AxisList[1].Range.Min := 0;
    Chart1.AxisList[1].Range.UseMax := True;
    Chart1.AxisList[1].Range.Max := 255;
    Chart1.AxisList[1].Title.Caption := 'Nivel de intensidad (0-255)';
    
    // Eje Y (frecuencia)
    Chart1.AxisList[0].Title.Caption := 'Frecuencia';
  except
  end;
end;

procedure TFormHist.ShowHistogram(const histData: THistogramData; colorMode: Integer = 0);
var
  i: Integer;
begin
  // Configurar las leyendas según el modo de color
  if colorMode = 3 then  // Modo HSV
  begin
    Chart1LineSeries2.Title := 'Matiz (H)';
    Chart1LineSeries3.Title := 'Saturación (S)';
    Chart1LineSeries4.Title := 'Brillo (V)';
  end
  else  // Modo RGB
  begin
    Chart1LineSeries2.Title := 'Rojo';
    Chart1LineSeries3.Title := 'Verde';
    Chart1LineSeries4.Title := 'Azul';
  end;
  Chart1LineSeries1.Title := 'Intensidad';
  
  // Limpiar series
  Chart1LineSeries1.Clear;
  Chart1LineSeries2.Clear;
  Chart1LineSeries3.Clear;
  Chart1LineSeries4.Clear;
  
  // Agregar datos
  for i := 0 to 255 do
  begin
    Chart1LineSeries2.AddXY(i, histData.Red[i]);
    Chart1LineSeries3.AddXY(i, histData.Green[i]);
    Chart1LineSeries4.AddXY(i, histData.Blue[i]);
    Chart1LineSeries1.AddXY(i, histData.Intensity[i]);
  end;
  
  Chart1.Invalidate;
  Self.Show;
end;

end.
