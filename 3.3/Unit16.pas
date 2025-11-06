unit Unit16;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, OpenGL, Math;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    DC: HDC;
    RC: HGLRC;
    Angle: GLfloat;
    TextureID: GLuint;
    procedure InitOpenGL;
    procedure SetupProjection;
    procedure RenderScene;
    procedure SetDCPixelFormat(hdc: HDC);
    procedure CreateChessTexture8x8;
    procedure DrawTexturedCube;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

const
  TexWidth = 512;
  TexHeight = 512;

var
  ArrayRGB: array[0..TexHeight-1, 0..TexWidth-1, 0..2] of GLubyte;

procedure TForm1.SetDCPixelFormat(hdc: HDC);
var
  pfd: TPixelFormatDescriptor;
  pf: Integer;
begin
  FillChar(pfd, SizeOf(pfd), 0);
  pfd.nSize := SizeOf(pfd);
  pfd.nVersion := 1;
  pfd.dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
  pfd.iPixelType := PFD_TYPE_RGBA;
  pfd.cColorBits := 24;
  pfd.cDepthBits := 16;
  pfd.iLayerType := PFD_MAIN_PLANE;
  pf := ChoosePixelFormat(hdc, @pfd);
  SetPixelFormat(hdc, pf, @pfd);
end;

procedure TForm1.CreateChessTexture8x8;
var
  BmpLight, BmpDark: TBitmap;
  i, j, cellW, cellH, xCell, yCell: Integer;
  c: TColor;
begin
  BmpLight := TBitmap.Create;
  BmpDark := TBitmap.Create;
  try
    BmpLight.LoadFromFile('C:\Users\Darina\Documents\Embarcadero\Studio\Projects\Лаб 4\Light.bmp');
    BmpDark.LoadFromFile('C:\Users\Darina\Documents\Embarcadero\Studio\Projects\Лаб 4\Dark.bmp');
    BmpLight.PixelFormat := pf24bit;
    BmpDark.PixelFormat := pf24bit;

    cellW := TexWidth div 8;
    cellH := TexHeight div 8;

    for i := 0 to TexWidth-1 do
      for j := 0 to TexHeight-1 do
      begin
        xCell := i div cellW;
        yCell := j div cellH;
        if (xCell + yCell) mod 2 = 0 then
          c := BmpLight.Canvas.Pixels[i mod BmpLight.Width, j mod BmpLight.Height]
        else
          c := BmpDark.Canvas.Pixels[i mod BmpDark.Width, j mod BmpDark.Height];

        ArrayRGB[j,i,0] := GetRValue(c);
        ArrayRGB[j,i,1] := GetGValue(c);
        ArrayRGB[j,i,2] := GetBValue(c);
      end;

    glGenTextures(1, @TextureID);
    glBindTexture(GL_TEXTURE_2D, TextureID);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, TexWidth, TexHeight,
                 0, GL_RGB, GL_UNSIGNED_BYTE, @ArrayRGB);
  finally
    BmpLight.Free;
    BmpDark.Free;
  end;
end;

procedure TForm1.InitOpenGL;
begin
  DC := GetDC(Handle);
  SetDCPixelFormat(DC);
  RC := wglCreateContext(DC);
  wglMakeCurrent(DC, RC);

  glClearColor(0.0, 0.0, 0.15, 1.0);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_TEXTURE_2D);

  CreateChessTexture8x8;

  SetupProjection;
end;

procedure TForm1.SetupProjection;
var
  aspect: GLfloat;
begin
  if ClientHeight = 0 then aspect := 1 else aspect := ClientWidth / ClientHeight;
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluPerspective(60.0, aspect, 1.0, 100.0);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;

procedure TForm1.DrawTexturedCube;
const
  h = 1.0;
begin
  glBindTexture(GL_TEXTURE_2D, TextureID);

  glBegin(GL_QUADS);

  glTexCoord2f(1,1); glVertex3f( h,  h,  h);
  glTexCoord2f(0,1); glVertex3f(-h,  h,  h);
  glTexCoord2f(0,0); glVertex3f(-h, -h,  h);
  glTexCoord2f(1,0); glVertex3f( h, -h,  h);

  glTexCoord2f(1,1); glVertex3f(-h,  h, -h);
  glTexCoord2f(0,1); glVertex3f( h,  h, -h);
  glTexCoord2f(0,0); glVertex3f( h, -h, -h);
  glTexCoord2f(1,0); glVertex3f(-h, -h, -h);

  glTexCoord2f(1,1); glVertex3f(-h,  h,  h);
  glTexCoord2f(0,1); glVertex3f(-h,  h, -h);
  glTexCoord2f(0,0); glVertex3f(-h, -h, -h);
  glTexCoord2f(1,0); glVertex3f(-h, -h,  h);

  glTexCoord2f(1,1); glVertex3f( h,  h, -h);
  glTexCoord2f(0,1); glVertex3f( h,  h,  h);
  glTexCoord2f(0,0); glVertex3f( h, -h,  h);
  glTexCoord2f(1,0); glVertex3f( h, -h, -h);

  glTexCoord2f(1,1); glVertex3f( h,  h, -h);
  glTexCoord2f(0,1); glVertex3f(-h,  h, -h);
  glTexCoord2f(0,0); glVertex3f(-h,  h,  h);
  glTexCoord2f(1,0); glVertex3f( h,  h,  h);

  glTexCoord2f(1,1); glVertex3f( h, -h,  h);
  glTexCoord2f(0,1); glVertex3f(-h, -h,  h);
  glTexCoord2f(0,0); glVertex3f(-h, -h, -h);
  glTexCoord2f(1,0); glVertex3f( h, -h, -h);
  glEnd;
end;

procedure TForm1.RenderScene;
begin
  if (RC = 0) or (DC = 0) then Exit;
  wglMakeCurrent(DC, RC);

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity;

  glTranslatef(0.0, 0.0, -6.0);
  glRotatef(Angle, 1.0, 1.0, 0.0);

  DrawTexturedCube;

  SwapBuffers(DC);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitOpenGL;
  Angle := 0.0;
  Timer1.Interval := 16;
  Timer1.Enabled := True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if RC <> 0 then
  begin
    wglMakeCurrent(0,0);
    wglDeleteContext(RC);
  end;
  if DC <> 0 then
    ReleaseDC(Handle, DC);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  if RC = 0 then Exit;
  wglMakeCurrent(DC, RC);
  glViewport(0,0, ClientWidth, ClientHeight);
  SetupProjection;
  RenderScene;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  RenderScene;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Angle := Angle + 1;
  if Angle >= 360 then Angle := 0;
  RenderScene;
end;

end.

