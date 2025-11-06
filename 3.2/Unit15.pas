unit Unit15;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, OpenGL;

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
    procedure SetDCPixelFormat(hdc: HDC);
    procedure InitOpenGL;
    procedure SetupProjection;
    procedure Draw3D;
    procedure LoadBmpTexture(const FileName: string);
  public
  end;

var
  Form1: TForm1;
  arrayRGB: array[0..511, 0..511, 0..2] of GLubyte;

implementation

{$R *.dfm}

var
  Bmp: TBitmap;

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

procedure TForm1.LoadBmpTexture(const FileName: string);
var
  i, j: Integer;
  c: TColor;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.LoadFromFile(FileName);
    Bmp.PixelFormat := pf24bit;

    for i := 0 to 511 do
      for j := 0 to 511 do
      begin
        c := Bmp.Canvas.Pixels[i, 511 - j];
        arrayRGB[j, i, 0] := GetRValue(c);
        arrayRGB[j, i, 1] := GetGValue(c);
        arrayRGB[j, i, 2] := GetBValue(c);
      end;

    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, 512, 512, 0, GL_RGB, GL_UNSIGNED_BYTE, @arrayRGB);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);

    glEnable(GL_TEXTURE_2D);
  finally
    Bmp.Free;
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
  glShadeModel(GL_SMOOTH);
  SetupProjection;

  LoadBmpTexture('C:\Users\Darina\Documents\Embarcadero\Studio\Projects\Лаб 4\текстура1.bmp');
end;

procedure TForm1.SetupProjection;
var
  aspect: GLfloat;
begin
  if ClientHeight = 0 then aspect := 1
  else aspect := ClientWidth / ClientHeight;

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluPerspective(45.0, aspect, 1.0, 100.0);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;

procedure TForm1.Draw3D;
const
  h = 1.0;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity;
  glTranslatef(0.0, 0.0, -6.0);
  glRotatef(Angle, 1.0, 1.0, 0.0);

  glBegin(GL_QUADS);

  glTexCoord2f(1, 1); glVertex3f( h,  h,  h);
  glTexCoord2f(0, 1); glVertex3f(-h,  h,  h);
  glTexCoord2f(0, 0); glVertex3f(-h, -h,  h);
  glTexCoord2f(1, 0); glVertex3f( h, -h,  h);

  glTexCoord2f(1, 1); glVertex3f(-h,  h, -h);
  glTexCoord2f(0, 1); glVertex3f( h,  h, -h);
  glTexCoord2f(0, 0); glVertex3f( h, -h, -h);
  glTexCoord2f(1, 0); glVertex3f(-h, -h, -h);

  glTexCoord2f(1, 1); glVertex3f(-h,  h,  h);
  glTexCoord2f(0, 1); glVertex3f(-h,  h, -h);
  glTexCoord2f(0, 0); glVertex3f(-h, -h, -h);
  glTexCoord2f(1, 0); glVertex3f(-h, -h,  h);

  glTexCoord2f(1, 1); glVertex3f( h,  h, -h);
  glTexCoord2f(0, 1); glVertex3f( h,  h,  h);
  glTexCoord2f(0, 0); glVertex3f( h, -h,  h);
  glTexCoord2f(1, 0); glVertex3f( h, -h, -h);

  glTexCoord2f(1, 1); glVertex3f( h,  h, -h);
  glTexCoord2f(0, 1); glVertex3f(-h,  h, -h);
  glTexCoord2f(0, 0); glVertex3f(-h,  h,  h);
  glTexCoord2f(1, 0); glVertex3f( h,  h,  h);

  glTexCoord2f(1, 1); glVertex3f( h, -h,  h);
  glTexCoord2f(0, 1); glVertex3f(-h, -h,  h);
  glTexCoord2f(0, 0); glVertex3f(-h, -h, -h);
  glTexCoord2f(1, 0); glVertex3f( h, -h, -h);

  glEnd;

  SwapBuffers(DC);
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
    wglMakeCurrent(0, 0);
    wglDeleteContext(RC);
    RC := 0;
  end;
  if DC <> 0 then
    ReleaseDC(Handle, DC);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  if RC = 0 then Exit;
  glViewport(0, 0, ClientWidth, ClientHeight);
  SetupProjection;
  Draw3D;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  Draw3D;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Angle := Angle + 1;
  if Angle >= 360 then Angle := 0;
  Draw3D;
end;

end.

