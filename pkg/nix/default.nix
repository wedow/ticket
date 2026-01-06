{
  lib,
  stdenv,
  fetchFromGitHub ? null,
  makeWrapper,
  bash,
  coreutils,
  findutils,
  gnused,
  gawk,
  gnugrep,
  ripgrep ? null,
  jq ? null,
  # Allow source override for flake usage
  source ? null,
  version ? "0.1.0",
}:
stdenv.mkDerivation rec {
  pname = "ticket";
  inherit version;

  src =
    if source != null
    then source
    else
      fetchFromGitHub {
        owner = "mojotech";
        repo = "ticket";
        rev = "v${version}";
        hash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";
      };

  nativeBuildInputs = [makeWrapper];

  dontBuild = true;

  installPhase = ''
    runHook preInstall
    install -Dm755 ticket $out/bin/tk
    install -Dm644 LICENSE $out/share/licenses/${pname}/LICENSE
    runHook postInstall
  '';

  postFixup = ''
    wrapProgram $out/bin/tk \
      --prefix PATH : ${lib.makeBinPath ([
        bash
        coreutils
        findutils
        gnused
        gawk
        gnugrep
      ]
      ++ lib.optionals (ripgrep != null) [ripgrep]
      ++ lib.optionals (jq != null) [jq])}
  '';

  meta = with lib; {
    description = "Minimal ticket tracking in bash";
    longDescription = ''
      The git-backed issue tracker for AI agents. Stores tickets as markdown
      files with YAML frontmatter in .tickets/ directory.
    '';
    homepage = "https://github.com/mojotech/ticket";
    license = licenses.mit;
    maintainers = ["github/wedow" "github/mojotech"];
    mainProgram = "tk";
    platforms = platforms.unix;
  };
}
