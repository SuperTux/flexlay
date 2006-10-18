// Code taken from the Gimp

#include <iostream>
#include <math.h>
#include <string.h>

#define OVERSAMPLING 5

typedef int gint;
typedef unsigned char guchar;

struct TempBuf {
  gint      width;
  gint      height;

  guchar   *data;       /*  The data buffer. Do never access this field
                            directly, use temp_buf_data() instead !!       */
};

void
temp_buf_free (TempBuf *temp_buf)
{
  // FIXME:
  delete temp_buf;
}

guchar *
temp_buf_data (TempBuf *temp_buf)
{
  return temp_buf->data;
}

TempBuf *
temp_buf_new (gint    width,
	      gint    height,
	      gint    bytes,
	      gint    x,
	      gint    y,
	      guchar *col)
{
  TempBuf* buf = new TempBuf;

  buf->data   = new guchar[width*height];
 
  buf->width  = width;
  buf->height = height;

  return buf;
}

#define gimp_deg_to_rad(angle) ((angle) * (2.0 * M_PI) / 360.0)
#define gimp_rad_to_deg(angle) ((angle) * 360.0 / (2.0 * M_PI))

static double
gauss (double f)
{
  /* this aint' a real gauss function */
  if (f < -0.5)
    {
      f = -1.0 - f;
      return (2.0 * f*f);
    }

  if (f < 0.5)
    return (1.0 - 2.0 * f*f);

  f = 1.0 - f;
  return (2.0 * f*f);
}

enum GimpBrushGeneratedShape  /*< pdb-skip >*/
{
  GIMP_BRUSH_GENERATED_CIRCLE,  /*< desc="Circle"  >*/
  GIMP_BRUSH_GENERATED_SQUARE,  /*< desc="Square"  >*/
  GIMP_BRUSH_GENERATED_DIAMOND  /*< desc="Diamond" >*/
};

struct GimpVector2
{
  double x, y;
};

struct GimpBrushGenerated
{
  TempBuf      *mask;       /*  the actual mask                */

  GimpVector2   x_axis;     /*  for calculating brush spacing  */
  GimpVector2   y_axis;     /*  for calculating brush spacing  */

  GimpBrushGeneratedShape shape;
  float                  radius;
  gint                    spikes;       /* 2 - 20     */
  float                  hardness;     /* 0.0 - 1.0  */
  float                  aspect_ratio; /* y/x        */
  float                  angle;        /* in degrees */
};


static void
gimp_brush_generated_dirty (GimpBrushGenerated *brush)
{
  gint                x, y;
  guchar             *centerp;
  double             d;
  double             exponent;
  guchar              a;
  gint                length;
  gint                width  = 0;
  gint                height = 0;
  guchar             *lookup;
  double             sum;
  double             c, s, cs, ss;
  double             short_radius;
  double             buffer[OVERSAMPLING];

  if (brush->mask)
    temp_buf_free (brush->mask);

  s = sin (gimp_deg_to_rad (brush->angle));
  c = cos (gimp_deg_to_rad (brush->angle));

  short_radius = brush->radius / brush->aspect_ratio;

  brush->x_axis.x =        c * brush->radius;
  brush->x_axis.y = -1.0 * s * brush->radius;
  brush->y_axis.x =        s * short_radius;
  brush->y_axis.y =        c * short_radius;

  switch (brush->shape)
    {
    case GIMP_BRUSH_GENERATED_CIRCLE:
      width  = static_cast<int>(ceil (sqrt (brush->x_axis.x * brush->x_axis.x +
                                            brush->y_axis.x * brush->y_axis.x)));
      height = static_cast<int>(ceil (sqrt (brush->x_axis.y * brush->x_axis.y +
                                            brush->y_axis.y * brush->y_axis.y)));
      break;

    case GIMP_BRUSH_GENERATED_SQUARE:
      width  = static_cast<int>(ceil (fabs (brush->x_axis.x) + fabs (brush->y_axis.x)));
      height = static_cast<int>(ceil (fabs (brush->x_axis.y) + fabs (brush->y_axis.y)));
      break;

    case GIMP_BRUSH_GENERATED_DIAMOND:
      width  = static_cast<int>(ceil (std::max(fabs (brush->x_axis.x), fabs (brush->y_axis.x))));
      height = static_cast<int>(ceil (std::max(fabs (brush->x_axis.y), fabs (brush->y_axis.y))));
      break;

    default:
      return;
    }

  if (brush->spikes > 2)
    {
      /* could be optimized by respecting the angle */
      width = height = static_cast<int>(ceil (sqrt (brush->radius * brush->radius +
                                                    short_radius * short_radius)));
      brush->y_axis.x =        s * brush->radius;
      brush->y_axis.y =        c * brush->radius;
    }

  brush->mask = temp_buf_new (width  * 2 + 1,
                               height * 2 + 1,
                               1, width, height, NULL);

  centerp = temp_buf_data (brush->mask) + height * brush->mask->width + width;

  /* set up lookup table */
  length = static_cast<int>(OVERSAMPLING * ceil (1 + sqrt (2 *
                                                           ceil (brush->radius + 1.0) *
                                                           ceil (brush->radius + 1.0))));

  if ((1.0 - brush->hardness) < 0.0000004)
    exponent = 1000000.0;
  else
    exponent = 0.4 / (1.0 - brush->hardness);

  lookup = new guchar[length];
  sum = 0.0;

  for (x = 0; x < OVERSAMPLING; x++)
    {
      d = fabs ((x + 0.5) / OVERSAMPLING - 0.5);

      if (d > brush->radius)
        buffer[x] = 0.0;
      else
        buffer[x] = gauss (pow (d / brush->radius, exponent));

      sum += buffer[x];
    }

  for (x = 0; d < brush->radius || sum > 0.00001; d += 1.0 / OVERSAMPLING)
    {
      sum -= buffer[x % OVERSAMPLING];

      if (d > brush->radius)
        buffer[x % OVERSAMPLING] = 0.0;
      else
        buffer[x % OVERSAMPLING] = gauss (pow (d / brush->radius, exponent));

      sum += buffer[x % OVERSAMPLING];
      lookup[x++] = static_cast<int>(rint(sum * (255.0 / OVERSAMPLING)));
    }

  while (x < length)
    {
      lookup[x++] = 0;
    }

  cs = cos (- 2 * M_PI / brush->spikes);
  ss = sin (- 2 * M_PI / brush->spikes);

  /* for an even number of spikes compute one half and mirror it */
  for (y = (brush->spikes % 2 ? -height : 0); y <= height; y++)
    {
      for (x = -width; x <= width; x++)
        {
          double tx, ty, angle;

          tx = c*x - s*y;
          ty = fabs (s*x + c*y);

          if (brush->spikes > 2)
            {
              angle = atan2 (ty, tx);

              while (angle > M_PI / brush->spikes)
                {
                  double sx = tx, sy = ty;

                  tx = cs * sx - ss * sy;
                  ty = ss * sx + cs * sy;

                  angle -= 2 * M_PI / brush->spikes;
                }
            }

          ty *= brush->aspect_ratio;
          switch (brush->shape)
            {
            case GIMP_BRUSH_GENERATED_CIRCLE:
              d = sqrt (tx*tx + ty*ty);
              break;
            case GIMP_BRUSH_GENERATED_SQUARE:
              d = std::max (fabs (tx), fabs (ty));
              break;
            case GIMP_BRUSH_GENERATED_DIAMOND:
              d = fabs (tx) + fabs (ty);
              break;
            }

          if (d < brush->radius + 1)
            a = lookup[(gint) rint (d * OVERSAMPLING)];
          else
            a = 0;

          centerp[ y * brush->mask->width + x] = a;

          if (brush->spikes % 2 == 0)
            centerp[-1 * y * brush->mask->width - x] = a;
        }
    }

  delete lookup;
}

int main()
{
  GimpBrushGenerated brush;
  
  brush.mask         = 0;
  brush.shape        = GIMP_BRUSH_GENERATED_DIAMOND;
  brush.radius       = 512;
  brush.spikes       = 19;
  brush.hardness     = 0.9;
  brush.aspect_ratio = 1;
  brush.angle        = 0;

  gimp_brush_generated_dirty(&brush);

  std::cout << "P2\n";
  std::cout << "# Gimp Brush Generator\n";
  std::cout << brush.mask->width << " " << brush.mask->height << "\n";
  std::cout << "255\n";

  for (int i = 0; i < brush.mask->width * brush.mask->height; ++i)
    std::cout << int(brush.mask->data[i]) << " ";

  temp_buf_free(brush.mask);
  
  std::cout << std::endl;

  return 0;
}

/* EOF */
