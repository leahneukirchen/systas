/* tag: Tom Lord Tue Dec  4 14:41:55 2001 (=config.c)
 */

int
stack_grows_up (unsigned long l)
{
  int x;
  return (l < ((unsigned long)&x));
}

int
main (int argc, char ** argv)
{
  int q;

  if (sizeof (long) == sizeof (float))
    {
      static char def[] = "#define SCM_SINGLES	1\n";
      write (1, def, strlen (def));
    }

  if (stack_grows_up ((unsigned long)&q))
    {
      static char def[] = "#define SCM_STACK_GROWS_UP\n";
      write (1, def, strlen (def));
    }
  exit (0);
}
