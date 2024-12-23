
namespace csharp;

public class UnitTest1
{
    [Fact]
    public void TestPart1Sample()
    {
        var a = GetData("../../../sample.txt");
        int result = Part1(a);
        Assert.Equal(18, result);
    }

    [Fact]
    public void TestPart1Input()
    {
        var a = GetData("../../../input.txt");
        Assert.Equal(2560, Part1(a));
    }

    [Fact]
    public void TestPart2Sample()
    {
        var a = GetData("../../../sample2.txt");
        Assert.Equal(9, Part2(a));
    }

    [Fact]
    public void TestPart2Input()
    {
        var a = GetData("../../../input.txt");
        Assert.Equal(1910, Part2(a));
    }
    private static int Part2(string[] a)
    {
        var l1 = a.Length;
        var l2 = a[0].Length;
        var result = 0;
        for (var i = 1; i < l1 - 1; i++)
        {
            for (var j = 1; j < l2 - 1; j++)
            {
                if (a[i][j] == 'A')
                {
                    char upperLeft = a[i - 1][j - 1];
                    char bottomRight = a[i + 1][j + 1];
                    if (upperLeft == 'M' && bottomRight == 'S' || upperLeft == 'S' && bottomRight == 'M')
                    {
                        char upperRight = a[i - 1][j + 1];
                        char bottomLeft = a[i + 1][j - 1];
                        if (upperRight == 'M' && bottomLeft == 'S' || upperRight == 'S' && bottomLeft == 'M')
                        {
                            result++;
                        }
                    }
                }
            }
        }

        return result;
    }

    private static int Part1(string[] a)
    {
        var l1 = a.Length;
        var l2 = a[0].Length;
        var result = 0;
        var word = "XMAS";
        (int, int)[] horizontalDirections = [(0, -1), (0, 1)];
        (int, int)[] verticalDirections = [(1, 0), (-1, 0)];
        (int, int)[] rightDiagonalDirections = [(-1, 1), (1, -1)];
        (int, int)[] leftDiagonalDirections = [(-1, -1), (1, 1)];
        for (var i = 0; i < l1; i++)
        {
            for (var j = 0; j < l2; j++)
            {
                result += Dfs(a, word, 0, l1, l2, horizontalDirections, i, j);
                result += Dfs(a, word, 0, l1, l2, verticalDirections, i, j);
                result += Dfs(a, word, 0, l1, l2, rightDiagonalDirections, i, j);
                result += Dfs(a, word, 0, l1, l2, leftDiagonalDirections, i, j);
            }
        }

        return result;
    }

    private static int Dfs(string[] a, string word, int pos, int l1, int l2, (int, int)[] directions, int i, int j)
    {
        if (i < 0 || j < 0 || i >= l1 || j >= l2) return 0;
        char candidate = a[i][j];
        char c = word[pos];
        if (c != candidate) return 0;

        if (pos == word.Length - 1) return 1;

        var result = 0;
        foreach (var (x, y) in directions)
        {
            result += Dfs(a, word, pos + 1, l1, l2, directions, i + x, j + y);
        }
        return result;
    }

    private static string[] GetData(string filePath)
    {
        return File.ReadAllLines(filePath);
    }
}