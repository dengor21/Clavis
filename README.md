# Clavis

Clavis is a VIA compatible layout manager for QMK keyboards written in Haskell.

## Features
- Listing connected VIA compatible keyboards.
- Saving your current keyboard layout.
- Flashing a modified keyboard layout.

## How it works
Clavis uses a subset of the VIA Api to reconfigure your keyboard.

To allow you to reconfigure your keylayout, Clavis generates a JSON file containing information about your keyboard.
VendorId and ProductId are checked before writing to the keyboard.
The Macro field is a placeholder for now.

Clavis uses the layout information in the VIA reference json to order the keymappings in the json list.
This should roughly resemble your physical keyboard layout.
There is one layer object per layer your keyboard has.

```json
{
    {
      "formatVersion": 1, 
      "keyboardName": "Keychron Q11 ANSI Knob", 
      "vid": "0x3438", 
      "pid": "0x01E0", 
      "macros": [],
      "layers": [
        {
          "layer": 0,
          "keys": [
            ["mute", "esc", "briu", "brid", "MCtrl", "LPad", "rgb_vad", "rgb_vai", "mprv", "play", "mnxt", "mute", "vold", "volu", "ins", "del", "mute"],
            ["macro(3)", "`", "1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "-", "=", "bspace", "pgup"],
            ["macro(4)", "tab", "q", "w", "e", "r", "t", "y", "u", "i", "o", "p", "[", "]", "\\", "pgdn"],
            ["macro(5)", "esc", "a", "s", "d", "f", "g", "h", "j", "k", "l", ";", "'", "enter", "home"],
            ["macro(6)", "lsft", "z", "x", "c", "v", "b", "n", "m", ",", ".", "/", "rsft", "up"],
            ["NumLock", "lctl", "LOpt", "LCmd", "fn1", "spc", "spc", "RCmd", "fn1", "rctl", "left", "down", "right"]
          ]
        },
      ]
    }
}
```

You can use short names for the most common keys, if your key is not in the dictionary, you can enter the hex code directly like `"0x53"` or `"53"`.

To generate a file with your current layout use `clavis yank`

**Yank**

```
clavis yank reference.json outputLayout.json

or to yank a specific layer

clavis yank reference.json outputLayout.json --layer 0
```

You can then edit the file in any text editor.

To reconfigure your keyboard use `clavis put`

**Put**

```
clavis put reference.json changedLayout.json

or to write a specific layer

clavis put reference.json changedLayout.json --layer 0
```

## VIA Reference JSON

Clavis needs a VIA-compatible reference JSON for your keyboard. This JSON contains layout and hardware information for your keyboard.
The ProductID and VendorID are used to connect to the keyboard via USB HID and the layout information are used to transpose the electrical grid into a more physical layout.
This is the same file that the manufacturer provides for VIA support.

You can usually find this file:

- On your manufacturer's Downloads/Support page
- On the [VIA keyboard database](https://github.com/the-via/keyboards)

## Supported keycodes

Keycodes not in the table below are preserved as raw hex values (e.g. `0x7F00`) and will round-trip correctly. Custom keycodes defined in your reference JSON via the `customKeycodes` field are picked up automatically.

./src/Keycodes.hs contains a full list.

## Installation

### Pre-built binary

Download the static Linux binary from the [latest release](https://github.com/dengor21/Clavis/releases/latest).

### Nix

```bash
nix run github:dengor21/Clavis -- yank reference.json layout.json
```

Or install persistently:

```bash
nix profile install github:dengor21/Clavis
```

## Building

The project ships a Nix flake with a dev shell that provides all dependencies and tools.
This is the supported build environment

```bash
# Dev shell + cabal
nix develop
cabal build
cabal run clavis -- yank reference.json layout.json
```

## Tests
The project includes HSpec tests.
You can run the tests with cabal.

```bash
cabal test
```

## License
BSD-3-Clause
