unit Unit17;

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
  private
    DC: HDC;
    RC: HGLRC;
    Angle: GLfloat;
    procedure SetDCPixelFormat(hdc: HDC);
    procedure InitOpenGL;
    procedure SetupProjection;
    procedure RenderScene;
    procedure DrawCube;
    procedure CreateDynamicTexture;
  public
    TexID: GLuint;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

const
  TexWidth = 256;
  TexHeight = 256;

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

procedure TForm1.InitOpenGL;
begin
  DC := GetDC(Handle);
  SetDCPixelFormat(DC);
  RC := wglCreateContext(DC);
  wglMakeCurrent(DC, RC);

  glClearColor(0.0, 0.0, 0.2, 1.0);
  glEnable(GL_DEPTH_TEST);
  glShadeModel(GL_SMOOTH);
  glEnable(GL_TEXTURE_2D);

  TexID := 1;
  CreateDynamicTexture;

  SetupProjection;
end;

procedure TForm1.SetupProjection;
var
  aspect: GLfloat;
begin
  if ClientHeight = 0 then aspect := 1
  else aspect := ClientWidth / ClientHeight;

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluPerspective(45.0, aspect, 0.1, 100.0);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;

procedure TForm1.CreateDynamicTexture;
var
  TexData: array[0..TexHeight-1, 0..TexWidth-1, 0..2] of GLubyte;
  i, j: Integer;
  x, y, r, theta, val: Double;
begin
  for i := 0 to TexWidth - 1 do
    for j := 0 to TexHeight - 1 do
    begin
      x := 2.0 * i / (TexWidth - 1) - 1.0;
      y := 2.0 * j / (TexHeight - 1) - 1.0;
      r := Sqrt(x*x + y*y);
      theta := ArcTan2(y, x);

      val := 0.5 + 0.5 * Sin(20*r + 15*theta + 5*Sin(10*x) + 5*Cos(10*y));

      TexData[j,i,0] := Round(val * 255);
      TexData[j,i,1] := Round((1-val) * 128);
      TexData[j,i,2] := Round(val * 200);
    end;

  glBindTexture(GL_TEXTURE_2D, TexID);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, TexWidth, TexHeight, 0,
               GL_RGB, GL_UNSIGNED_BYTE, @TexData);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
end;

procedure TForm1.DrawCube;
const
  h = 1.0;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity;
  glTranslatef(0.0, 0.0, -6.0);
  glRotatef(Angle, 1.0, 1.0, 0.0);

  glBindTexture(GL_TEXTURE_2D, TexID);

  glBegin(GL_QUADS);

    glTexCoord2f(0,0); glVertex3f(-h,-h, h);
    glTexCoord2f(1,0); glVertex3f( h,-h, h);
    glTexCoord2f(1,1); glVertex3f( h, h, h);
    glTexCoord2f(0,1); glVertex3f(-h, h, h);

    glTexCoord2f(0,0); glVertex3f(-h,-h,-h);
    glTexCoord2f(1,0); glVertex3f( h,-h,-h);
    glTexCoord2f(1,1); glVertex3f( h, h,-h);
    glTexCoord2f(0,1); glVertex3f(-h, h,-h);

    glTexCoord2f(0,0); glVertex3f(-h,-h,-h);
    glTexCoord2f(1,0); glVertex3f(-h,-h, h);
    glTexCoord2f(1,1); glVertex3f(-h, h, h);
    glTexCoord2f(0,1); glVertex3f(-h, h,-h);

    glTexCoord2f(0,0); glVertex3f(h,-h,-h);
    glTexCoord2f(1,0); glVertex3f(h,-h, h);
    glTexCoord2f(1,1); glVertex3f(h, h, h);
    glTexCoord2f(0,1); glVertex3f(h, h,-h);

    glTexCoord2f(0,0); glVertex3f(-h,h,-h);
    glTexCoord2f(1,0); glVertex3f( h,h,-h);
    glTexCoord2f(1,1); glVertex3f( h,h, h);
    glTexCoord2f(0,1); glVertex3f(-h,h, h);

    glTexCoord2f(0,0); glVertex3f(-h,-h,-h);
    glTexCoord2f(1,0); glVertex3f( h,-h,-h);
    glTexCoord2f(1,1); glVertex3f( h,-h, h);
    glTexCoord2f(0,1); glVertex3f(-h,-h, h);

  glEnd;

  SwapBuffers(DC);
end;

procedure TForm1.RenderScene;
begin
  DrawCube;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitOpenGL;
  Angle := 0;
  Timer1.Interval := 16;
  Timer1.Enabled := True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if RC <> 0 then
  begin
    wglMakeCurrent(0,0);
    wglDeleteContext(RC);
    RC := 0;
  end;
  if DC <> 0 then
  begin
    ReleaseDC(Handle, DC);
    DC := 0;
  end;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  if RC = 0 then Exit;
  wglMakeCurrent(DC,RC);
  glViewport(0,0,ClientWidth,ClientHeight);
  SetupProjection;
  RenderScene;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Angle := Angle + 1.5;
  if Angle >= 360 then Angle := 0;
  RenderScene;
end;

end.

