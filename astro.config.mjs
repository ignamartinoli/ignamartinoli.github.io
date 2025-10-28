// @ts-check

import mdx from '@astrojs/mdx';
import sitemap from '@astrojs/sitemap';
import { defineConfig } from 'astro/config';
import rehypeKatex from 'rehype-katex';
import rehypeSlug from 'rehype-slug';
import remarkMath from 'remark-math';
import remarkToc from 'remark-toc';

import tailwindcss from '@tailwindcss/vite';

export default defineConfig({
	integrations: [mdx(), sitemap()],

	markdown: {
		rehypePlugins: [
			rehypeKatex,
			rehypeSlug,
		],
		remarkPlugins: [
			remarkMath,
			[remarkToc, {
				heading: false,
				maxDepth: 6,
				tight: true,
				ordered: false,
			}]
		],
		shikiConfig: {
			themes: {
				dark: 'catppuccin-macchiato',
				light: 'catppuccin-latte'
			}
		}
	},

	site: 'https://ignamartinoli.github.io',

	vite: {
		plugins: [tailwindcss()]
	}
});
