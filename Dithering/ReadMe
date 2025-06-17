# Haskell Image Dithering

This project provides a suite of classic image dithering algorithms implemented in Haskell. It features both a high-performance command-line interface (CLI) for batch processing and a full-featured graphical user interface (GUI) for interactive experimentation.

## Features

- **Multiple Algorithms:**
  - Floyd-Steinberg (Error Diffusion)
  - Atkinson (Error Diffusion)
  - Ordered Dithering (using a 4x4 Bayer matrix)
  - Simple Color Quantization (for comparison)
- **Adjustable Color Depth:** Control the number of bits per color channel (1-8 bits).
- **Selectable Color Metrics:** Choose between Euclidean and Manhattan distance for finding the nearest color in the palette.
- **Interactive GUI:**
  - Load any common image format (PNG, JPG, BMP, etc.).
  - Real-time preview of dithering results.
  - "Fit to Window" and manual zoom controls for easy viewing of large images.
  - Two-stage visual feedback: see the quantized image before the final dithered result appears.
- **High-Performance CLI:** A command-line tool for scripting and processing images without a graphical interface.

## Prerequisites

Before building this project, you must have the following installed on your system:

### 1. GHC and Cabal

The Haskell Toolchain, including the Glasgow Haskell Compiler (GHC) and the Cabal build tool. The recommended way to install this is using `ghcup`:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

Follow the on-screen instructions. This will install GHC and Cabal in your home directory and configure your shell's `PATH`.

### 2. GTK Development Libraries (for the GUI)

The graphical user interface is built using GTK. You need to install the C development libraries for GTK3 on your system.

**On Debian / Ubuntu / Linux Mint:**
```bash
sudo apt-get update
sudo apt-get install libgtk-3-dev build-essential gobject-introspection libgirepository1.0-dev
```

**On Fedora / CentOS / RHEL:**
```bash
sudo dnf install gtk3-devel gobject-introspection-devel
```

**On Arch Linux:**
```bash
sudo pacman -S gtk3 gobject-introspection
```

*(Note: The command-line version does not require these GTK libraries.)*

## Building the Project

Once the prerequisites are installed, building is a single command. Navigate to the root directory of the project (where the `Dithering.cabal` file is located) and run:

```bash
cabal build
```

Cabal will automatically download all required Haskell libraries, then compile the core dithering library and both the CLI and GUI executables. The first build may take some time as it compiles the GTK bindings.

## Running the Application

There are two executables you can run.

### 1. Running the GUI Version

This is the recommended way to use the application for interactive dithering.

To run the GUI, use the `cabal exec` command:
```bash
cabal exec dithering-gui
```
The application window will open. The workflow is as follows:
1.  Click **"Choose an Image"** to load a picture from your disk. The original image will be displayed.
2.  Adjust the **Algorithm**, **Palette Metric**, and **Bits per channel** using the controls on the right.
3.  Click the **"Dither!"** button to apply the selected settings. You will see the image first become quantized, and then a moment later, it will resolve into the final dithered version.
4.  Use the **Zoom** slider or **"Fit to Window"** checkbox to inspect the image.
5.  Click **"Save Dithered Image..."** to save the result.

### 2. Running the Command-Line Version

The CLI is ideal for scripting or batch processing. It takes the input file, an output name "root", and optional parameters for bit depth and color metric. It will generate a set of dithered images, one for each major algorithm.

**Usage:**
```bash
cabal exec dithering-cli -- input.png output-root [nBits] [euk|man]
```

**Examples:**

- **Basic Usage (defaults to 3 bits, Euclidean metric):**
  ```bash
  cabal exec dithering-cli -- /path/to/my/photo.jpg /tmp/dithered_photo
  ```
  This will create the following files in the `/tmp/` directory:
  - `dithered_photo_quant.png`
  - `dithered_photo_dither_fs.png` (Floyd-Steinberg)
  - `dithered_photo_dither_atkinson.png`
  - `dithered_photo_dither_ordered.png`

- **With Custom Settings (2 bits per channel, Manhattan metric):**
  ```bash
  cabal exec dithering-cli -- /path/to/my/photo.jpg /tmp/dithered_photo 2 man
  ```