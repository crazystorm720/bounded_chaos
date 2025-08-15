Below is a **meta-repo skeleton** for `φ-net`.  
It is **deliberately over-factored** so that:

- every primitive is independently testable  
- future emergent capabilities (e.g. WireGuard overlays, SBOM attestation, policy-as-rego) can be slotted in without re-touching the core  
- the repo itself becomes an **executable artifact** (`cue vet`, `cue export`, OCI bundle, Terraform data-source, etc.)

Directory tree first, then a short “why this shape” section.

```
phinet/
├── cue.mod/
│   ├── module.cue          // module definition
│   └── gen/                // generated CUE from external sources (optional)
├── spec/
│   ├── root/
│   │   └── network.cue     // #Network (single source-of-truth)
│   ├── security/
│   │   ├── cipher.cue      // #CipherSuite, #SecureShell
│   │   └── transport.cue   // #Port
│   ├── identity/
│   │   ├── atom.cue        // #Atom, #Tier
│   │   └── numbers.cue     // _prime, _fib
│   ├── topology/
│   │   ├── subnet.cue      // #Subnet, #Prefix, #IPv4
│   │   ├── node.cue        // #Node
│   │   └── dns.cue         // #RR
│   └── domain/
│       └── naming.cue      // #Domain
├── examples/
│   ├── minimal/
│   │   └── spec.cue        // smallest legal network
│   └── ha-edge/
│       └── spec.cue        // 3-tier, 2-subnet edge mesh
├── test/
│   ├── spec/               // unit tests for every leaf struct
│   │   ├── cipher_test.cue
│   │   ├── fib_test.cue
│   │   └── atom_test.cue
│   └── integration/
│       └── validate.cue    // `cue vet` over examples/
├── policy/                 // future: OPA/Rego, Cilium NetworkPolicy, etc.
├── overlays/               // future: WireGuard, VXLAN, eBPF programs
├── sbom/                   // future: SPDX/CycloneDX CUE schemas
├── ci/
│   ├── github/
│   │   └── workflows/
│   │       └── vet.yml     // `cue vet ./...`
│   └── docker/
│       └── Dockerfile      // thin image with cue + yq + jq
├── dist/                   // build outputs (OCI, JSON, TF variables)
│   └── bundle/
│       └── manifest.json   // `cue export` result
├── docs/
│   ├── rationale.md
│   └── diagrams/
├── LICENSE
└── README.md
```

---

### “Meta” design notes

1. **Single root contract**  
   `spec/root/network.cue` exports `#Network`.  
   Everything else is *private* unless explicitly re-exported; this lets us refactor internals without breaking downstream.

2. **Fractal modules**  
   Each sub-directory under `spec/` is a **micro-module**.  
   Consumers who only need `#CipherSuite` can do  
   ```
   cue get go github.com/you/phinet/spec/security@v0.4.0
   ```  
   and depend on a 30-line file instead of the entire mesh.

3. **Emergent-capability slots**  
   - `policy/` – drop Rego, CEL, or Cedar files here; CI will `opa test` or `cue import` them.  
   - `overlays/` – when you add WireGuard, keep the schema in `overlays/wg/wg.cue`; the root stays untouched.  
   - `sbom/` – when SPDX-in-CUE stabilises, add `sbom/spdx.cue` and extend `#Node` with an SPDX field via embedding.

4. **Build pipeline as code**  
   `ci/github/workflows/vet.yml` does  
   ```
   cue vet ./...
   cue export ./examples/... > dist/bundle/manifest.json
   ```  
   Committing `dist/bundle/manifest.json` makes the repo consumable by Terraform’s `http` data-source *without* requiring CUE in the end-user pipeline.

5. **Versioned schema but *content-addressable* artifacts**  
   Tags follow **semver** (`v0.4.0`) while the OCI bundle pushed to GHCR is  
   ```
   ghcr.io/you/phinet:sha-1a2b3c4
   ```  
   so you can pin either semver or SHA.

6. **Self-documenting**  
   `README.md` is auto-generated from `cue def #Network | cue fmt -s`.  
   `docs/diagrams/` is rendered by `cueloader` + `d2` → SVG committed nightly.

---

### Quick start (copy-paste)

```bash
git init phinet
cd phinet
mkdir -p cue.mod/spec/{root,security,identity,topology,domain} \
         examples/{minimal,ha-edge} test/{spec,integration} \
         policy overlays sbom ci/{github/workflows,docker} dist/bundle docs/diagrams
```

Drop the **refactored CUE fragments** from the earlier message into the appropriate files, add a `LICENSE` (Apache-2.0 is idiomatic), push to GitHub, tag `v0.1.0`, and you’re done.

Future emergent capabilities now have a **place to land instead of a rewrite to endure**.
