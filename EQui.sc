// ht to Wouter Snoei
EQui : QUserView {
	var <params, <target, prefix, sampleRate;

	*viewClass { ^QUserView }

	*new { arg parent, bounds, target, params, prefix = "", sampleRate;
		^super.new(parent, bounds).init(target, params, prefix, sampleRate);
	}

	*sizeHint {
		^Point(300,200);
	}

	target_ {|intarget| target = intarget; target.set(*params.asArgsArray(prefix)); }

	params_ {|inparams| params = inparams; this.refresh; this.doAction }

	init {|intarget, inparams, inprefix, insr|
		var selected = -1;
		var downX, downY;
		params = inparams ?? { EQuiParams() };
		target = intarget;
		prefix = inprefix;
		sampleRate = insr ?? {Server.default.sampleRate};

		this.drawFunc = { |vw|
			var freqs, svals, values, bounds, zeroline;
			var min = 20, max = 22050, range = 24;
			var vlines = [100,1000,10000];
			var dimvlines = [25,50,75, 250,500,750, 2500,5000,7500];
			var hlines = [-18,-12,-6,6,12,18];
			var pt, strOffset = 11;

			bounds = vw.bounds.moveTo(0,0);

			freqs = ({|i| i } ! (bounds.width+1));
			freqs = freqs.linexp(0, bounds.width, min, max );

			values = [
				BLowShelf.magResponse( freqs, sampleRate, params.loShelfFreq, params.loShelfRs,
					params.loShelfGain),
				BPeakEQ.magResponse( freqs, sampleRate, params.loPeakFreq, params.loPeakRq,
					params.loPeakGain),
				BPeakEQ.magResponse( freqs, sampleRate, params.midPeakFreq, params.midPeakRq,
					params.midPeakGain),
				BPeakEQ.magResponse( freqs, sampleRate, params.hiPeakFreq, params.hiPeakRq,
					params.hiPeakGain),
				BHiShelf.magResponse( freqs, sampleRate, params.hiShelfFreq, params.hiShelfRs,
					params.hiShelfGain)
			].ampdb.max(-200).min(200);

			zeroline = 0.linlin(range.neg,range, bounds.height, 0, \none);

			svals = values.sum.linlin(range.neg,range, bounds.height, 0, \none);
			values = values.linlin(range.neg,range, bounds.height, 0, \none);

			vlines = vlines.explin( min, max, 0, bounds.width );
			dimvlines = dimvlines.explin( min, max, 0, bounds.width );

			pt = 5.collect({ |ind|
				(params.freqByIndex(ind).explin( min, max, 0, bounds.width ))
				@
				(params.gainByIndex(ind).linlin(range.neg,range,bounds.height,0,\none));
			});

			Pen.color_( Color.white.alpha_(0.25) );
			Pen.roundedRect( bounds, [6,6,0,0] ).fill;

			Pen.color = Color.gray(0.2).alpha_(0.5);

			Pen.roundedRect( bounds.insetBy(0,0), [6,6,0,0] ).clip;

			Pen.color = Color.gray(0.2).alpha_(0.125);

			hlines.do({ |hline,i|
				hline = hline.linlin( range.neg,range, bounds.height, 0, \none );
				Pen.line( 0@hline, bounds.width@hline )
			});
			dimvlines.do({ |vline,i|
				Pen.line( vline@0, vline@bounds.height );
			});
			Pen.stroke;

			Pen.color = Color.gray(0.2).alpha_(0.5);
			vlines.do({ |vline,i|
				Pen.line( vline@0, vline@bounds.height );
			});
			Pen.line( 0@zeroline, bounds.width@zeroline ).stroke;

			Pen.font = Font( Font.defaultSansFace, 10 );

			Pen.color = Color.gray(0.2).alpha_(0.5);
			hlines.do({ |hline|
				Pen.stringAtPoint( hline.asString ++ "dB",
					3@(hline.linlin( range.neg,range, bounds.height, 0, \none )
						- strOffset) );
			});
			vlines.do({ |vline,i|
				Pen.stringAtPoint( ["100Hz", "1KHz", "10KHz"][i],
					(vline+2)@(bounds.height - (strOffset + 1)) );
			});

			if( selected != -1, {
				var string, strBounds;

				string = [ "low shelf: %hz, %dB, rs=%",
					"peak 1: %hz, %dB, rq=%",
					"peak 2: %hz, %dB, rq=%",
					"peak 3: %hz, %dB, rq=%",
					"hi shelf: %hz, %dB, rs=%"
				][ selected ].format(
					params.freqByIndex(selected),
					params.gainByIndex(selected),
					params.bwByIndex(selected)
				);
				strBounds = string.bounds(Pen.font);
				strBounds.top = 1;
				strBounds.left = this.bounds.width - 1 - strBounds.width;
				Pen.color = Color.white.alpha_(0.8);
				Pen.addRect(strBounds);
				Pen.fill;
				Pen.stringInRect(string, strBounds, Pen.font, Color.gray(0.2).alpha_(0.5), \right);
			});

			values.do({ |svals,i|
				var color;
				color = Color.hsv(
					i.linlin(0,values.size,0,1),
					0.75, 0.5).alpha_(if( selected == i ) { 0.75 } { 0.25 });
				Pen.color = color;
				Pen.moveTo( 0@(svals[0]) );
				svals[1..].do({ |val, i|
					Pen.lineTo( (i+1)@val );
				});
				Pen.lineTo( bounds.width@(bounds.height/2) );
				Pen.lineTo( 0@(bounds.height/2) );
				Pen.lineTo( 0@(svals[0]) );
				Pen.fill;

				Pen.addArc( pt[i], 5, 0, 2pi );

				Pen.color = color.alpha_(0.75);
				Pen.stroke;

			});

			Pen.color = Color.blue(0.5);
			Pen.moveTo( 0@(svals[0]) );
			svals[1..].do({ |val, i|
				Pen.lineTo( (i+1)@val );
			});
			Pen.stroke;

		};
		this.refresh;

		this.mouseDownAction = { |vw,x,y,mod|
			var bounds;
			var pt;
			var min = 20, max = 22050, range = 24;

			bounds = vw.bounds.moveTo(0,0);
			pt = (x@y);

			selected =  (0..4).detect({ |index|
				(( params.freqByIndex(index).explin( min, max, 0, bounds.width ) )@
					( params.gainByIndex(index).linlin( range.neg, range, bounds.height, 0, \none ) ))
				.dist( pt ) <= 5;
			}) ? -1;
			downX = x;
			downY = y;
			vw.refresh;
		};

		this.mouseMoveAction = { |vw,x,y,mod|
			var bounds;
			var pt;
			var min = 20, max = 22050, range = 24;

			bounds = vw.bounds.moveTo(0,0);
			pt = (x@y);

			if( selected != -1 )
			{
				case { mod.isAlt }
				{
					params.setBwByIndex(selected,
						(y - downY + params.bwByIndex(selected)
							.explin( 0.1, 10, bounds.height, 0, \none ))
						.linexp( bounds.height, 0, 0.1, 10, \none )
						.clip(if( [0,4].includes(selected) ) { 0.6 } {0.1},
						10).round(0.01));

				}
				{ true }
				{
					params.setFreqByIndex(selected, pt.x.linexp(0, bounds.width, min, max ).clip(20,20000).round(1));
					params.setGainByIndex(selected, pt.y.linlin( 0, bounds.height, range, range.neg, \none ).clip2( range ).round(0.25));
				};

				this.doAction;
				vw.refresh;

			};


		}
	}

	doAction {
		action.value(this, params, prefix);
		target.set(*params.asArgsArray(prefix));
	}
}

EQuiParams {
	var <>loShelfFreq = 100, <>loShelfGain = 0, <>loShelfRs = 1;
	var <>loPeakFreq = 250, <>loPeakGain = 0, <>loPeakRq = 1;
	var <>midPeakFreq = 1000, <>midPeakGain = 0, <>midPeakRq = 1;
	var <>hiPeakFreq = 3500, <>hiPeakGain = 0, <>hiPeakRq = 1;
	var <>hiShelfFreq = 6000, <>hiShelfGain = 0, <>hiShelfRs = 1;
	classvar bands = #[\loShelf, \loPeak, \midPeak, \hiPeak, \hiShelf];

	asArgsArray {|prefix|
		^[ 'loShelfFreq', 'loShelfGain', 'loShelfRs', 'loPeakFreq', 'loPeakGain', 'loPeakRq', 'midPeakFreq', 'midPeakGain', 'midPeakRq', 'hiPeakFreq', 'hiPeakGain', 'hiPeakRq', 'hiShelfFreq', 'hiShelfGain', 'hiShelfRs' ].collectAs({|key| (prefix ++ key).asSymbol->this.perform(key) }, IdentityDictionary).asArgsArray;
	}

	freqByIndex{|index| ^this.perform((bands[index] ++ 'Freq').asSymbol) }
	gainByIndex{|index| ^this.perform((bands[index] ++ 'Gain').asSymbol) }
	bwByIndex{|index|
		var suffix;
		suffix = if(index.inRange(1, 3), {"Rq"}, {"Rs"});
		^this.perform((bands[index] ++ suffix).asSymbol)
	}

	setFreqByIndex{|index, val| this.perform((bands[index] ++ 'Freq_').asSymbol, val) }
	setGainByIndex{|index, val| this.perform((bands[index] ++ 'Gain_').asSymbol, val) }
	setBwByIndex{|index, val|
		var suffix;
		suffix = if(index.inRange(1, 3), {"Rq_"}, {"Rs_"});
		this.perform((bands[index] ++ suffix).asSymbol, val)
	}

}

+ UGen {

	equi {|params, prefix = "", lag = 0.1|
		var chain, lagCtl;
		params = params ?? {EQuiParams()}; // defaults
		chain = this;
		lagCtl = Control.names([prefix ++ "lagEQ"]).kr(lag);

		chain = BLowShelf.ar( chain,
			NamedControl.kr(prefix ++ "loShelfFreq", params.loShelfFreq, lagCtl),
			NamedControl.kr(prefix ++ "loShelfRs", params.loShelfRs, lagCtl),
			NamedControl.kr(prefix ++ "loShelfGain", params.loShelfGain, lagCtl)
		);
		chain = BPeakEQ.ar( chain,
			NamedControl.kr(prefix ++ "loPeakFreq", params.loPeakFreq, lagCtl),
			NamedControl.kr(prefix ++ "loPeakRq", params.loPeakRq, lagCtl),
			NamedControl.kr(prefix ++ "loPeakGain", params.loPeakGain, lagCtl)
		);
		chain = BPeakEQ.ar( chain,
			NamedControl.kr(prefix ++ "midPeakFreq", params.midPeakFreq, lagCtl),
			NamedControl.kr(prefix ++ "midPeakRq", params.midPeakRq, lagCtl),
			NamedControl.kr(prefix ++ "midPeakGain", params.midPeakGain, lagCtl)
		);
		chain = BPeakEQ.ar( chain,
			NamedControl.kr(prefix ++ "hiPeakFreq", params.hiPeakFreq, lagCtl),
			NamedControl.kr(prefix ++ "hiPeakRq", params.hiPeakRq, lagCtl),
			NamedControl.kr(prefix ++ "hiPeakGain", params.hiPeakGain, lagCtl)
		);
		chain = BHiShelf.ar( chain,
			NamedControl.kr(prefix ++ "hiShelfFreq", params.hiShelfFreq, lagCtl),
			NamedControl.kr(prefix ++ "hiShelfRs", params.hiShelfRs, lagCtl),
			NamedControl.kr(prefix ++ "hiShelfGain", params.hiShelfGain, lagCtl)
		);
		chain = RemoveBadValues.ar( chain );

		^chain;
	}
}

+ SequenceableCollection {

	equi { arg ... args; ^this.multiChannelPerform('equi', *args) }

}