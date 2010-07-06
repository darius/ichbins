/^;/ { next; }
/^[ \t\r]*$/ { if ("" == action) next; }

/^' / { # '
        start("'"); input = $0; next; }
/^- / { start("-"); input = substr($0, 3); next; }
/^[*] / { expected = vert(expected, substr($0, 3)); next; }

/^[=!] / { expect($0, run_on(input)); input = action = ""; next; }

{ input = input "\n" $0 }

function start(c)
{
  expected = "";
  if ("" != input) {
    print "unused input";
    exit(1);
  }
  action = c;
}

function run_on(input)
{
  print input >"input";
  close("input")
  system("./lump <input >output");
  return trim(snarf("output"));
}

function expect(expected_last, output,   prefix)
{
  prefix = substr(expected_last, 1, 2);
  expected_last = substr(expected_last, 3);
  expected = prefix vert(expected, expected_last);
  if (expected !~ /\n/)
    gsub(/\n/, "~", output);
  if (output != expected) {
    printf("%d: mismatch: %s\n", NR, output);
    printf("%d: expected: %s\n", NR, expected);
  }
}

function vert(line1, line2)
{
  if (line1 == "") return line2;
  return line1 "\n" line2;
}

function trim(s)
{
  if (0 < length(s) && substr(s, length(s), 1) == "\n")
    s = substr(s, 1, length(s) - 1);
  return s;
}

function snarf(file,    result, line)
{
  result = "";
  while (getline line <file)
    result = result "\n" line;
  close(file);
  return substr(result, 2);
}
