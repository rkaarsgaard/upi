List of claims
==============
The only claim made in the paper regarding the implementation is found on page 2, lines 73-74: "All constructions and translations in this paper are formalised in (heavily extended) Glasgow Haskell." Concretely, this refers to implementations of UPi (Sec. 3.1-3.2), UPi_a (Sec. 3.3), and UPi^chi_a (Sec. 3.4), as well as the translation of (noniterative) quantum flow charts into UPi^chi_a (Sec. 6.2). Implicit in this claim is that derived combinators are well typed.

The implementation does not engage in any sort of theorem proving.


Download, installation, and sanity-testing
==========================================
The artifact comes in the form of a VirtualBox image running Ubuntu 20.04.3. The image was created using VirtualBox 6.1.26 r145957 running on macOS 11.6 (Big Sur). The reviewer can simply run this image, using the credentials below if prompted.

 * User: quantum
 * Password: quantum

The codebase itself is found in /home/quantum/upi. A suitable version of Glasgow Haskell is already installed, which the reviewer can verify by navigating to /home/quantum/upi in the terminal and running

    $ ghci QFC.hs

which should result in the following output:

    GHCi, version 8.6.5: https://www.haskell.org/ghc/  :? for help
    [1 of 7] Compiling UPiBase          ( UPiBase.hs, interpreted )
    [2 of 7] Compiling UPi              ( UPi.hs, interpreted )
    [3 of 7] Compiling UPiaBase         ( UPiaBase.hs, interpreted )
    [4 of 7] Compiling UPia             ( UPia.hs, interpreted )
    [5 of 7] Compiling UPichiaBase      ( UPichiaBase.hs, interpreted )
    [6 of 7] Compiling UPichia          ( UPichia.hs, interpreted )
    [7 of 7] Compiling QFC              ( QFC.hs, interpreted )
    Ok, 7 modules loaded.
    *QFC>
 

Evaluation instructions
=======================
The artifact has no executable components as such, but rather takes the form of a library implementing each language as a GADT, with derived combinators and combinator transformers implemented as Haskell constants and functions of suitable type.

Since the artifact only implements constructions and translations, and that the only real claim to test is that derived combinators are well-typed, evaluate the artifact as follows:

Verify that the GADTs for each language (in UPiBase.hs, UPiaBase.hs, UPichiaBase.hs) faithfully describe its syntax and type system.
Verify that derived combinators (in UPi.hs, UPia.hs, UPichia.hs) are specified as described in the paper (in Sections 3.1-3.2, 3.3, and 3.4 respectively).
Verify that the translation of quantum flow charts to UPichia (in QFC.hs) is specified as described in the paper (in Section 6.2).
Verify that all files typecheck (i.e., one is able to successfully load each file into GHCI without any error messages).

Additional artifact description
===============================
See /home/quantum/upi/README.md in the VirtualBox image for a description of the organisation of the codebase. The reviewer can interact with the implementation by loading up the appropriate file (e.g., UPi.hs) in GHCI.

Building from source
====================
The artifact can also be built from source if necessary. To do this, go to https://github.com/rkaarsgaard/upi and download the source, then run the relevant files using GHCI. We have successfully tested this on macOS Big Sur 11.6 running GHC 8.10.1 as well as Ubuntu 20.04 running GHC 8.6.5.
