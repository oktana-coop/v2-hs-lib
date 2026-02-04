# Notes Testing

It is always the case that certain foundational technologies and research, along of course with timing, play a role in shaping the terrain and the conditions of action. For us and this project of collaborative rich text writing, it is very important that there is now an exciting movement in software development culture, [local-first software](https://www.inkandswitch.com/essay/local-first/), sparked by a foundational technology relevant to collaborative applications, [CRDTs](https://crdt.tech/). We are very much inspired by the relevant work developed and published by the [Ink and Switch](https://www.inkandswitch.com/) industrial research lab and the thoughtful discussions in the [Metamuse](https://museapp.com/podcast/) podcast. We found there this missing element of utopian thinking for computing and digital media, a fertile critique of what computers have become in contrast to their great potential for being tools for thought and co-creation. What's more, there has been important and concrete research conducted within Ink and Switch for rich text collaborative writing [^1] [^2] [^3], upon which we are building and which we aim to continue and extend. And we are very much aligned to the recurring theme that writing is a socio-technical process [^4], so alongside the technical challenges we have to talk to people to understand their workflows and also consider politico-economic factors and dynamics.

The product we are launching today in an early Alpha release is a **local-first rich text editor with version control capabilities, **[**v2**](https://v2editor.com "v2"). In the following paragraphs we'll try to unpack this high-level description and highlight some important principles, design decisions and their consequences.

### Focused, Non-Distracted Thinking

Creative thinking and action, knowledge and artistic work, although sometimes perceived as floating apart from time or space, takes place through concrete, material mediums and processes: actual people produce it combining their physical and mental activity with tools that enable, facilitate or impose constraints on it. And these tools have qualities and embody principles that leave their mark on the creative process and the artifacts themselves [^5]. And today's computing is all but helping us think and systematically apply knowledge: it favors superficial thinking, even completely resigning from the need to think by having a language model fake it. It constantly tries to steal our attention, distract it from thinking and convert it to a desire to buy, flooding our inboxes with notifications and ads, or even just making us do its chores [^6], leaving our mind in a state of exhaustion mixed with compulsion which Fisher calls *insomniac overstimulation* [^7]. And it is really no surprise if we consider Marx's critique of capitalism: machines become an alienating and dominating force to the worker, depriving them of all interest and creative activity, as best manifested in the factory floor [^8] but really ubiquitous to all other domains or production, including knowledge and art. In contemporary capitalism, this also extends to leisure activity, which increasingly resembles labor and produces data that big tech capitalizes on.

v2 is the first expression of our desire to reverse this trend, as it culturally belongs to the emerging ecosystem of *tools for thought*. Such tools aim to enhance and augment the creative process and help us focus without distractions on the thoughts that matter, often just by offering an environment of serenity [^9]. This is why we consciously avoid cluttering the user interface with ribbons and a million controls that would distract us from focused thinking and writing. And, of course, we will be very careful not to add redundant notifications as we are adding features related to collaboration. Finally, we are statutorily opposed to advertising, so our plan is to resist polluting our products with annoying ads.

[^1]: Geoffrey Litt, Sarah Lim, Martin Kleppmann, and Peter van Hardenberg. 2022. Peritext: A CRDT for Collaborative Rich Text Editing. Proc. ACM Hum.-Comput. Interact. 6, CSCW2, Article 531 (November 2022), 36 pages. https://doi.org/10.1145/3555644

[^2]: Karissa Rae McKelvey, Scott Jenson, Eileen Wagner, Blaine Cook, Martin Kleppmann, March 2023. Upwelling: Combining real-time collaboration with version control for writers. https://www.inkandswitch.com/upwelling/

[^3]: Geoffrey Litt, Paul Sonnentag, Max Schöning, Adam Wiggins, Peter van Hardenberg, Orion Henry. Patchwork: Version control for everything. February 2024. https://www.inkandswitch.com/patchwork/notebook/

[^4]: Sarah Lim, Marc McGranaghan, Sarah Lim's interview with Adam Wiggins and Mark McGranaghan, Metamuse Episode 48, Rich text with Slim Lim, 00:14:23, podcast audio, January 20, 2022, https://museapp.com/podcast/48-rich-text/.

[^5]: David Graeber, Toward An Anthropological Theory of Value, The False Coin of Our Own Dreams (Palgrave 2001), 54, 83

[^6]: Adam Wiggins, Molly Mielke's interview with Adam Wiggins and Marc McGranaghan, Metamuse Episode 30, Computers and Creativity, 00:25:30, podcast audio, May 12, 2021, https://museapp.com/podcast/30-computers-and-creativity/.

[^7]: Mark Fisher & Darren Ambrose (Editor), Time-Wars: Towards an Alternative for the Neo-Capitalist Era in K-Punk: The Collected and Unpublished Writings of Mark Fisher (Repeater Books 2018)

[^8]: Karl Marx, Capital, A Critique of Political Economy, Volume 1 (Penguin Books 1976), 548-49

[^9]: We are pretty aligned on the environment creative tools should offer with the vision Adam Wiggins and Mark McGranaghan present in Metamuse Episode 30 cited above, but also pervading their Metamuse podcast more generally.
