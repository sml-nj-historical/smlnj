union u { int i; };

void uf (union u u)
{
  return;
}

int f1 (void)
{
  return 1;
}

float f2 (void)
{
  return 2.0;
}

double f3 (void)
{
  return 3.0;
}

int addii (int x, int y)
{
  return x + y;
}

float addif (int x, float y)
{
  return x + y;
}

double addid (int x, double y)
{
  return x + y;
}

double adddi (double x, int y)
{
  return x + y;
}

double projid2 (int x, double y)
{
  return y;
}

double projsid3 (char *s, int x, double y)
{
  return y;
}
