using System.Text;
using csharp;

var input = File.ReadAllLines("../input.txt");

var wide = 101;
var tall = 103;

var found = false;
var times = 0;
var sb = new StringBuilder();
Console.Clear();
while (!found)
{
    sb.Clear();
    var inputs =
        input.Select(l =>
        {
            var (p, v) = UnitTest1.ParseLine(l);
            return UnitTest1.GetPos(p, v, times, wide, tall);

        });
    var h = new HashSet<(long, long)>(inputs);
    for (var y = 0; y < tall; y++)
    {
        for (var x = 0; x < wide; x++)
        {
            var c = h.Contains((x, y)) ? 'X' : '.';
            sb.Append(c);
        }
        sb.AppendLine();
    }

    var m = sb.ToString();

    Console.Clear();
    Console.WriteLine(m);
    Console.WriteLine(times);

    if (m.Contains("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")) found = true;
    if (!found) times++;
}

File.WriteAllText($"../{times}.txt", sb.ToString());
Console.ReadKey();