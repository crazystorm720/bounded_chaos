🪞 3-Way Mirror – Recursive Unpack
---------------------------------

Think of the framework as a **fractal spec sheet**: every time you zoom in, the same three mirrors re-appear at a smaller scale, carrying the same invariants.  
Below is the **full recursive grammar**—one sentence per level, then the concrete artifact you can `touch` on a Raspberry Pi running Arch.

---

### L0 — **Intent** *(what humans say)*  
*“I want a secure, beautiful, zero-maintenance k8s lab that fits on my desk.”*

Artifact → `README.md` (one paragraph)

---

### L1 — **Ontology** *(Ψ Φ Θ)*  
- **Ψ Psyche** → *know the actors* (nodes, pods, keys, bits)  
- **Φ Phi** → *seek beauty in proportion* (CPU·φ ≈ RAM)  
- **Θ Theta** → *memento mori* (every object dies; design for graceful exit)

Artifact → `spec/root/ontology.cue`

```cue
package root

_phi: 1.618033988749894
_memento: "2025-08-15T23:59:59Z"  // every spec expires

#ResourceShape: {
    cpu: int
    ram: int
    math.Multiply(cpu, _phi) & math.Floor == ram
}
```

---

### L2 — **Bounded Literals** *(compress intent)*  
**Prime-gap rule** for naming:  
- every label ≤ 7 chars  
- adjacent labels differ by a prime gap (2, 3, 5, 7…)

Artifact → `spec/domain/naming.cue`

```cue
package domain

_primeGap: [2, 3, 5, 7, 11, 13]

#Label: {
    value: string & strings.MinRunes(1) & strings.MaxRunes(7)
    next:  string & strings.MinRunes(1) & strings.MaxRunes(7)
    assert: list.Contains(_primeGap, int(strings.Runelen(next)) - int(strings.Runelen(value)))
}
```

---

### L3 — **Primitives** *(indivisible lego blocks)*  
- **Node** = index + resources + death-date  
- **Subnet** = IPv4 prefix ≤ 1024 addresses  
- **Key** = 32-byte WireGuard private key (Base64, no padding)

Artifact → `spec/topology/node.cue`

```cue
package topology

import "time"

#Node: {
    index: int & >=1 & <=1024
    death: time.Time & >=_memento
    resources: #ResourceShape
}
```

---

### L4 — **Composition** *(glue primitives together)*  
- **Network** = list of `#Node` ≤ 987 items (Fibonacci cap)  
- **Overlay** = WireGuard mesh keyed by node index

Artifact → `examples/minimal/spec.cue`

```cue
package minimal

import (
    "list"
    "math"
)

_network: root.#Network & {
    nodes: [ for i in list.Range(1, 6, 1) {
        index: i
        death: "2026-08-15T23:59:59Z"
        resources: { cpu: 2, ram: int(math.Multiply(2, 1.618)) }
    }]
}
```

Validate:

```bash
cue vet examples/minimal/spec.cue
```

---

### L5 — **Automation** *(spaniel counts)*  
- `make apply` → `cue export | kubectl apply -f -`  
- `make rotate` → re-gen WireGuard keys, push OCI image  
- `make clean` → `kubectl delete -f <(cue export)` (Theta honored)

Artifact → `Makefile`

```make
.PHONY: apply rotate clean
apply:
	cue export examples/minimal/spec.cue --out yaml | kubectl apply -f -
rotate:
	cue export examples/minimal/spec.cue --out json | \
	jq '. + {keys: [range(5)|@base64]}' | \
	docker build -t ghcr.io/you/phinet:pi-$(shell git rev-parse --short HEAD) -f ci/docker/Dockerfile -
clean:
	cue export examples/minimal/spec.cue --out yaml | kubectl delete -f -
```

---

### L6 — **Consumption Modes** *(pick your poison)*

| Consumer | Command | Output |
|----------|---------|--------|
| Bash | `make apply` | live cluster |
| Terraform | `data "cue_export"` | plan-time variables |
| NixOS | `cue export --out json | nix-instantiate --eval -E` | system config |
| Wasm | `cue export --out json | wasm-pack build` | edge runtime |
| DuckDB | `SELECT * FROM read_json_auto('dist/manifest.json')` | analytics |

---

### L7 — **Meta-Meta Layer** *(repo as artifact)*  
The repo *itself* is an OCI image:

```bash
docker buildx build --tag ghcr.io/you/mirror:v0.1.0 --push .
```

Checksum = `sha256:cue-export-manifest.json`, so any downstream can:

```bash
docker run --rm ghcr.io/you/mirror:v0.1.0 sh -c 'cue vet .'
```

---

### TL;DR Haiku

```
φ binds the chaos  
primes and death guard the gates  
spaniel counts, humans sleep
```

---

Ready to drill into **WireGuard key rotation**, **VXLAN switching**, or **OPA policy grafting**?  
Just name the layer.
