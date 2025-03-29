---
title: What we know about Qwen3 so far
date: 2025-03-29
logo: qwen
lang: en
---

**Disclaimer**: This article was written with the help of an LLM.

**Last update**: Saturday 29th of March 2025

The following information was derived from ongoing work aiming at adding support for soon-to-be-released Qwen3 to various projects:
- https://github.com/huggingface/transformers/pull/36878
- https://github.com/vllm-project/vllm/pull/15289
- https://github.com/InternLM/lmdeploy/pull/3305
- https://github.com/sgl-project/sglang/pull/4693

# What We Know About Qwen3 So Far

**Qwen3** is a next-generation large language model in the Qwen family, offering a standard Transformer decoder design with the potential for long-context processing. Below, we present both confirmed information as well as some deductions and open questions about what `Qwen3` (and its MoE counterpart, `Qwen3MoE`) might bring to the table.

## 1) Confirmed Information About Qwen3

1. **Model & Variants**
   - The primary model name is `Qwen3`, with a Mixture-of-Experts variant: `Qwen3MoE`.
   - Documentation mentions an *8B* model size for Qwen3 and a *15B* size for Qwen3MoE (with 2B active parameters).

2. **Core Architecture**
   - Qwen3 uses a *decoder-only Transformer* structure.
   - It employs *rotary position embeddings (RoPE)* to encode positions.
   - *RMSNorm* is used instead of the more common LayerNorm, and there is also an applied *QK-norm* step to the query/key projections.
   - Each layer consists of self-attention plus an MLP feed-forward block, with the MoE variant swapping in MoE blocks for some layers.

3. **Default Hyperparameters (Base Qwen3)**
   - *Vocabulary size*: 151,936
   - *Hidden dimension*: 4096
   - *Intermediate dimension*: 22,016
   - *Number of layers*: 32
   - *Attention heads*: 32, each head dimension at 128
   - *Max position embeddings*: up to 32,768 tokens
   - *RoPE base*: 10,000.0
   - Typically no bias in the Q, K, or V projections, with an option for attention bias if desired.
   - “Sliding window” attention can optionally be configured in lower layers (usually 4096 tokens in the “window”).

4. **MoE-Specific Attributes (Qwen3MoE)**
   - Substitutes certain MLP blocks with MoE layers.
   - Additional hyperparameters include the number of experts (128), top-k per-token routing (8), and a router loss factor (0.001), among others.
   - Can optionally output gating information (`router_logits`) that indicates token routing among experts.

5. **Extended Context Handling**
   - The maximum sequence length is set to 32k tokens, suggesting large-context capabilities.
   - A “sliding window” mode can be turned on for some layers, indicating a goal of more efficient attention for extended sequences.

6. **Multiple Task Heads**
   - Qwen3 supports a causal language modeling head for text generation, as well as heads for sequence classification, token classification, and QA tasks.

7. **Licensing & Release Notes**
   - Operates under the **Apache 2.0 License**.
   - Official usage guidelines, pretrained weights, and final performance claims are not yet included, with placeholders indicating a future public release.

## 2) Additional Deductions (Not Fully Confirmed)

1. **Parameter Counts**
   - The code references 8B parameters for Qwen3 and ~15B for Qwen3MoE, but there may be other configurations or final parameter counts beyond these documented examples.

2. **Performance and Training Data**
   - The data used, training schedule, and final performance are not yet public. No training steps or official evaluations have been confirmed.

3. **Release Timeline**
   - While mention is made of an upcoming model launch, no definitive timeline or final usage documentation is yet available.

4. **Tokenizer & Vocabulary**
   - Qwen3 apparently leverages a large vocabulary of over 150k tokens, likely similar to a previous iteration. Full details on backward compatibility or merges remain unspecified.

5. **Use Cases**
   - The architecture supports classification, QA, and generative tasks, but real-world usage details, recommended domain coverage, or specialized fine-tuning steps remain unknown.

### Conclusion

*Qwen3* (and *Qwen3MoE*) appear to be successors to the [Qwen2.5 family of models](https://qwenlm.github.io/blog/qwen2.5/), aiming for extensive context handling (at least 32k tokens) and improved efficiency via sliding-window attention or MoE routing. The primary *Qwen3* version is sized around 8B parameters, while *Qwen3MoE* is reportedly 15B (2B active parameters), leveraging 128 experts and specialized routing. Both variants incorporate rotary position embeddings, RMSNorm, and an optional QK-norm step in attention. Although official performance metrics and training data details remain undisclosed, these architectural choices suggest strong capabilities for text generation, classification, and QA across large-scale language tasks.
