{
  lib,
  fetchFromGitHub,
  python3Packages,
  qt5,
}:

python3Packages.buildPythonPackage {
  pname = "quadrocontrol";
  version = "0.1";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "leoratte";
    repo = "aquacomputer-quadro-control";
    rev = "1bfbe3d4400b329264bd6e1b6ca4ce6ddef4f41f";
    hash = "sha256-TPAk0L1owcOM6HpQeMY7jX4XaFE1n8xEsZhz6Zwzd/A=";
  };

  build-system = [ python3Packages.hatchling ];
  dependencies = with python3Packages; [
    crccheck
    pyqt5
    pyusb
  ];

  nativeBuildInputs = [ qt5.wrapQtAppsHook ];
  buildInputs = [ qt5.qtwayland ];
  dontWrapQtApps = true;
  preFixup = ''
    makeWrapperArgs+=("''${qtWrapperArgs[@]}")
  '';

  # Upstream's example is not valid Python and is not part of the application.
  postPatch = ''
    rm src/quadrocontrol/example.py
  '';

  pythonImportsCheck = [ "quadrocontrol.quadrocontrol" ];

  meta = {
    description = "GUI control for the Aqua Computer QUADRO fan controller";
    homepage = "https://github.com/leoratte/aquacomputer-quadro-control";
    license = lib.licenses.gpl3Only;
    mainProgram = "quadro-control";
    platforms = lib.platforms.linux;
  };
}
