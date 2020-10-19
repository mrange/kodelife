// Pulse team logo in neonwave
Texture2D shaderTexture;
SamplerState samplerState;

cbuffer PixelShaderSettings {
  float  Time;
  float  Scale;
  float2 Resolution;
  float4 Background;
};

#define PI          3.141592654
#define TAU         (2.0*PI)
#define RESOLUTION  Resolution
#define TIME        Time

#define SCA(a) float2(sin(a), cos(a))

static const float2 sca0 = SCA(0.0);

void rot(inout float2 p, float a) {
  float c = cos(a);
  float s = sin(a);
  p = float2(c*p.x + s*p.y, -s*p.x + c*p.y);
}

float mod1(inout float p, float size) {
  float halfsize = size*0.5;
  float c = floor((p + halfsize)/size);
  p = abs(p + halfsize)%size - halfsize;
  return c;
}

float pmin(float a, float b, float k) {
  float h = clamp( 0.5+0.5*(b-a)/k, 0.0, 1.0 );
  return lerp( b, a, h ) - k*h*(1.0-h);
}

float pmax(float a, float b, float k) {
  return -pmin(-a, -b, k);
}

float planex(float2 p, float w) {
  return abs(p.y) - w;
}

float planey(float2 p, float w) {
  return abs(p.x) - w;
}

float circle(float2 p, float r) {
  return length(p) - r;
}

float box(float2 p, float2 b) {
  float2 d = abs(p)-b;
  return length(max(d,0.0)) + min(max(d.x,d.y),0.0);
}

float horseshoe(float2 p, float2 c, float r, float2 w) {
  p.x = abs(p.x);
  float l = length(p);
  p = mul(p, float2x2(-c.x, c.y, c.y, c.x));
  p = float2((p.y>0.0)?p.x:l*sign(-c.x),(p.x>0.0)?p.y:l);
  p = float2(p.x,abs(p.y-r))-w;
  return length(max(p,0.0)) + min(0.0,max(p.x,p.y));
}

float letterp(float2 p) {
  float b = box(p - float2(-0.45, -0.25), float2(0.1, 0.75));
  float c = max(circle(p, 0.5), -circle(p, 0.3));
  return min(b, c);
}

float letteru(float2 p) {
  return horseshoe(p - float2(0.0, 0.125), sca0, 0.5, float2(0.375, 0.1));
}

float letterl(float2 p) {
  return box(p, float2(0.125, 0.75));
}

float letters(float2 p) {
  rot(p, -PI/6.0);
  rot(p, -PI/2.0);
  float u = horseshoe(p - float2(-0.25*3.0/4.0, -0.125/2.0), sca0, 0.375, float2(0.2, 0.1)) - 0.0;
  rot(p, PI);
  float l = horseshoe(p - float2(-0.25*3.0/4.0, -0.125/2.0), sca0, 0.375, float2(0.2, 0.1));
  return min(u,l);
}

float lettere(float2 p) {
  return min(box(p, float2(0.4, 0.1)), max(circle(p, 0.5), -circle(p, 0.3)));
}

float pulse(float2 p) {
  p.x += 1.95;
  const float op = +0.10;
  const float ou = +1.25;
  const float ol = +2.10;
  const float os = +2.80;
  const float oe = +3.85;
  float dp = letterp(p - float2(op, 0.0));
  float du = letteru(p - float2(ou, 0.0));
  float dl = letterl(p - float2(ol, 0.0));
  float ds = letters(p - float2(os, 0.0));
  float de = lettere(p - float2(oe, 0.0));
  float oo = 0.1;
  float dx = abs(p.y) - oo;
  dx = abs(dx) - oo*0.5;
  float d = 1000000.0;
  d = min(d, dp);
  d = min(d, du);
  d = min(d, dl);
  d = min(d, ds);
  d = min(d, de);
  d = max(d, -dx);
  return d;
}

float sun(float2 p) {
  const float ch = 0.0125;
  float2 sp = p;
  float2 cp = p;
  mod1(cp.y, ch*6.0);
  float d0 = circle(sp, 0.5);
  float d1 = planex(cp, ch);
  float d2 = p.y+ch*3.0;
  float d = d0;
  d = pmax(d, -max(d1, d2), ch*2.0);
  return d;
}

float df(float2 p) {
  const float2 off = float2(0.0, -9.0);
  const float2 coff = float2(0.0, 0.0);
  const float si = 5.0;
  const float sc = 25.0;
  float di = pulse((p - off)/si)*si;
  float ds = sun(p/sc)*sc;
  float dh = max(planex(p - off, 0.25), -planey(p, 12.75));
  float d = ds;
  d = max(d, -(di - 1.2));
  d = min(d, di);
  d = min(d, dh);
  return d;
}

float3 f3(float v) {
  return float3(v, v, v);
}

float3 postProcess(float3 col, float2 q)  {
  col = clamp(col,0.0,1.0);
  col = tanh(2.0*col/3.0);
  col=col*0.6+0.4*col*col*(3.0-2.0*col);
  col=lerp(col, f3(dot(col, f3(0.33))), -0.4);
  col*=0.5+0.5*pow(19.0*q.x*q.y*(1.0-q.x)*(1.0-q.y),0.7);
  return col;
}

float3 effect(float2 q) {
  q.y = 1.0 - q.y;
  float2 p = -1. + 2. * q;
  p.x *= RESOLUTION.x/RESOLUTION.y;

  float aa = 4.0/RESOLUTION.y;

  float3 sunCol = lerp(float3(1.0, 1.0, 0.0), float3(1.0, 0.0, 1.0), clamp((0.85 - p.y)*0.75, 0.0, 1.0));
  float3 glareCol = sqrt(sunCol);
  float ss = smoothstep(-1.05, 0.0, p.y);
  float3 glow = lerp(float3(1.0, 0.7, 0.6).zyx, glareCol, ss);

  float s = 15.0;
  float d = df(p*s)/s;
  float db = abs(d) - 0.0025;

  float3 col = float3(1.0, 0.0, 1.0)*0.125;
  col += 0.65*glow*exp(-2.5*d)*ss;
  col = lerp(col, sunCol*ss, smoothstep(-aa, aa, -d));
  col = lerp(col, glow*1.55, smoothstep(-aa, aa, -db));

  col = postProcess(col, q);

  return col;
}

float4 main(float4 pos : SV_POSITION, float2 tex : TEXCOORD) : SV_TARGET {
  float4 t = shaderTexture.Sample(samplerState, tex);
  float3 e = effect(tex);

  float3 col = e;
  col = lerp(col, t.xyz, t.w);

  return float4(col, 1.0);
}
