using Tataru.Engine;

internal static class Program {
    internal static int Main(string[] args) {
        Console.OutputEncoding = System.Text.Encoding.UTF8;
        (List<string[]>? Result, ContextFetcherEnums Code) = Engine.FetchContext(["(foo + bar); (foo + bar); (foo + ", " bar]"], 0);
        return 0;
    }
}