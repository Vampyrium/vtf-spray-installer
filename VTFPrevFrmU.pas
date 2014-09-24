unit VTFPrevFrmU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VTFLib, Winapi.GDIPAPI, Vcl.ExtCtrls,
  Vcl.StdCtrls, Vcl.Samples.Spin, System.Actions, Vcl.ActnList;

type
  TVTFPrevForm = class(TForm)
    MipSelector: TSpinEdit;
    ScrollBox1: TScrollBox;
    VTFControlPnl: TPanel;
    VTFPaintBox: TPaintBox;
    AnimTimer: TTimer;
    Label1: TLabel;
    Label2: TLabel;
    FrameSelector: TSpinEdit;
    AnimEnable: TCheckBox;
    ActionList1: TActionList;
    AnimEnableAction: TAction;
    Shape1: TShape;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure VTFPaintBoxPaint(Sender: TObject);
    procedure MipSelectorChange(Sender: TObject);
    procedure AnimTimerTimer(Sender: TObject);
    procedure FrameSelectorChange(Sender: TObject);
    procedure AnimEnableActionExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    BufBMP: array of TBitmap;
    TransBkg: TBitmap;
    VTFImg: Cardinal;
    FrameCount, MipCount: Cardinal;
    RenderingFrame, RenderingMip: Cardinal;
    procedure RenderFrame(Frame, Face, Slice, Mipmap, DestBuf: Cardinal);
    procedure ClearBufBMP;
  public
    procedure LoadImage(FileName: ansistring);
  end;

var
  VTFPrevForm: TVTFPrevForm;

implementation

{$R *.dfm}

{$POINTERMATH ON}
procedure TVTFPrevForm.LoadImage(FileName: ansistring);
var
  i,j,count: NativeInt;
begin
  VTFLib.vlCreateImage(@VTFImg);
  VTFLib.vlBindImage(VTFImg);
  VTFLib.vlImageLoad(PAnsiChar(FileName),false);

  ClearBufBMP;

  FrameCount := VTFLib.vlImageGetFrameCount;
  MipCount := VTFLib.vlImageGetMipmapCount;

  SetLength(BufBMP,FrameCount * MipCount);

  RenderingFrame := 0;
  RenderingMip := 0;
  FrameSelector.Value := 0;
  MipSelector.Value := 0;

  count := 0;

  for j := 0 to MipCount - 1 do begin
    for i := 0 to FrameCount - 1 do begin
      BufBMP[count] := TBitmap.Create;
      BufBMP[count].HandleType := bmDIB;
      BufBMP[count].PixelFormat := pf32bit;
      BufBMP[count].AlphaFormat := afPremultiplied;//afDefined;
      RenderFrame(i,0,0,j,count);
      count := count + 1;
    end;
  end;

  VTFLib.vlDeleteImage(VTFImg);

  ClientWidth := BufBMP[0].Width + VTFControlPnl.Width;
  ClientHeight := BufBMP[0].Height;

  VTFPaintBox.Invalidate;
end;

procedure TVTFPrevForm.MipSelectorChange(Sender: TObject);
begin
  if MipSelector.Value > (MipCount - 1) then MipSelector.Value := MipCount - 1;
  if MipSelector.Value < 0 then MipSelector.Value := 0;
  RenderingMip := MipSelector.Value;
  VTFPaintBox.Invalidate;
end;

procedure TVTFPrevForm.RenderFrame(Frame, Face, Slice, Mipmap, DestBuf: Cardinal);
var
  i,j: Cardinal;
  DatPtr, pixmap: PByte;
  R,G,B,A: Byte;
  colorp: PColor;
begin
  BufBMP[DestBuf].Width := VTFLib.vlImageGetWidth shr Mipmap;
  BufBMP[DestBuf].Height := VTFLib.vlImageGetHeight shr Mipmap;

  DatPtr := VTFLib.vlImageGetData(Frame,Face,Slice,Mipmap);
  GetMem(pixmap,BufBMP[DestBuf].Width * BufBMP[DestBuf].Height * 4);
  VTFLib.vlImageConvertToRGBA8888(DatPtr,pixmap,VTFLib.vlImageGetWidth shr Mipmap,VTFLib.vlImageGetHeight shr Mipmap,VTFLib.vlImageGetFormat);

  for j := 0 to BufBMP[DestBuf].Height - 1 do begin
    colorp := BufBMP[DestBuf].ScanLine[j];
    for i := 0 to BufBMP[DestBuf].Width - 1 do begin
      A := pixmap[(j*BufBMP[DestBuf].Height*4)+(i*4)+3];
      R := (pixmap[(j*BufBMP[DestBuf].Height*4)+(i*4)] * A) div 255;
      G := (pixmap[(j*BufBMP[DestBuf].Height*4)+(i*4)+1] * A) div 255;
      B := (pixmap[(j*BufBMP[DestBuf].Height*4)+(i*4)+2] * A) div 255;
      colorp[i] :=
        (R shl 16) or
        (G shl 8) or
        (B) or
        (A shl 24);
    end;
  end;

  FreeMem(pixmap,BufBMP[DestBuf].Width * BufBMP[DestBuf].Height * 4);
end;

procedure TVTFPrevForm.VTFPaintBoxPaint(Sender: TObject);
var
  SelBuf: NativeUInt;
begin
  if Length(BufBMP) > 0 then begin

    //VTFPaintbox.Canvas.
    VTFPaintBox.Canvas.Brush.Color := clBtnFace;
    VTFPaintBox.Canvas.FillRect(VTFPaintBox.ClientRect);
    SelBuf := (RenderingMip * FrameCount) + RenderingFrame;
    //Winapi.GDIPAPI.
    //FBlendFunc.BlendOp := AC_SRC_OVER;
    //FBlendFunc.SourceConstantAlpha := 255;
    //FBlendFunc.AlphaFormat := AC_SRC_ALPHA;
    VTFPaintBox.Canvas.Draw(0,0,TransBkg,255);
    VTFPaintBox.Canvas.Draw(0,0,BufBMP[SelBuf],255);
    VTFPaintBox.Width := BufBMP[SelBuf].Width;
    VTFPaintBox.Height := BufBMP[SelBuf].Height;
    //Winapi.Windows.AlphaBlend(VTFPaintBox.Canvas.Handle,0,0,BufBMP[0].Width,BufBMP[0].Height,BufBMP[0].Canvas.Handle,
    //  0,0,BufBMP[0].Width,BufBMP[0].Height,FBlendFunc);
  end;
end;

procedure TVTFPrevForm.AnimEnableActionExecute(Sender: TObject);
begin
  AnimTimer.Enabled := AnimEnableAction.Checked;
end;

procedure TVTFPrevForm.AnimTimerTimer(Sender: TObject);
var
  prevframe: NativeUInt;
begin
  if Length(BufBMP) > 0 then begin
    prevframe := RenderingFrame;
    FrameSelector.Value := (RenderingFrame + 1) mod FrameCount;
  end;
end;

procedure TVTFPrevForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ClearBufBMP;
end;

procedure TVTFPrevForm.FormCreate(Sender: TObject);
var
  transcolors: array[0..1] of TColor;
  transcolor: NativeInt;
  i,j: NativeInt;
begin
  FrameCount := 0;
  MipCount := 0;
  SetLength(BufBMP,0);
  //LoadImage('C:\Users\Gage\Desktop\vampyrium1-hr.vtf');
  //LoadImage('C:\Program Files (x86)\Steam\SteamApps\common\Left 4 Dead 2\left4dead2\materials\vgui\logos\custom\asscumspray2-anim-hr.vtf');
  TransBkg := TBitmap.Create;
  TransBkg.SetSize(1024,1024);

  transcolors[0] := $00C0C0C0;
  transcolors[1] := $00FFFFFF;

  for j := 0 to (TransBkg.Height div 8) - 1 do begin
    transcolor := j mod 2;
    for i := 0 to (TransBkg.Width div 8) - 1 do begin
      TransBkg.Canvas.Brush.Color := transcolors[transcolor];
      TransBkg.Canvas.FillRect(Rect(i*8,j*8,i*8+8,j*8+8));
      transcolor := (transcolor + 1) mod 2;
    end;
  end;
end;

procedure TVTFPrevForm.FormDestroy(Sender: TObject);
var
  i: NativeInt;
begin
  for i := 0 to Length(BufBMP) - 1 do begin
    BufBMP[i].Free;
  end;
end;

procedure TVTFPrevForm.FrameSelectorChange(Sender: TObject);
begin
  if FrameSelector.Value > (FrameCount - 1) then FrameSelector.Value := FrameCount - 1;
  if FrameSelector.Value < 0 then FrameSelector.Value := 0;
  RenderingFrame := FrameSelector.Value;
  VTFPaintBox.Invalidate;
end;

procedure TVTFPrevForm.ClearBufBMP;
var
  i: NativeInt;
begin
  for i := 0 to Length(BufBMP) - 1 do begin
    BufBMP[i].Free;
  end;

  SetLength(BufBMP,0);

  FrameCount := 0;
  MipCount := 0;
end;

end.
