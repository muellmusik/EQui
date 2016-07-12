// ht to Wouter Snoei
EQui : QUserView {
	var <>params, <>target;

	*viewClass { ^QUserView }

	*new { arg parent, bounds;
		^super.new(parent, bounds).init;
	}

	*sizeHint {
		^Point(300,200);
	}

	init {
		var selected = -1;
		var downX, downY;
		params = [[100,0,1], [250,0,1], [1000,0,1], [3500,0,1], [6000,0,1]]; // init vals
		this.drawFunc = { |vw|
			var freqs, svals, values, bounds, zeroline;
			var freq = 1200, rq = 0.5, db = 12;
			var min = 20, max = 22050, range = 24;
			var vlines = [100,1000,10000];
			var dimvlines = [25,50,75, 250,500,750, 2500,5000,7500];
			var hlines = [-18,-12,-6,6,12,18];
			var pt, strOffset = 11;

			bounds = vw.bounds.moveTo(0,0);

			#freq,db,rq = params[0] ? [ freq, db, rq ];

			freqs = ({|i| i } ! (bounds.width+1));
			freqs = freqs.linexp(0, bounds.width, min, max );

			values = [
				BLowShelf.magResponse( freqs, 44100, params[0][0], params[0][2],
					params[0][1]),
				BPeakEQ.magResponse( freqs, 44100, params[1][0], params[1][2],
					params[1][1]),
				BPeakEQ.magResponse( freqs, 44100, params[2][0], params[2][2],
					params[2][1]),
				BPeakEQ.magResponse( freqs, 44100, params[3][0], params[3][2],
					params[3][1]),
				BHiShelf.magResponse( freqs, 44100, params[4][0], params[4][2],
					params[4][1])
			].ampdb.max(-200).min(200);

			zeroline = 0.linlin(range.neg,range, bounds.height, 0, \none);

			svals = values.sum.linlin(range.neg,range, bounds.height, 0, \none);
			values = values.linlin(range.neg,range, bounds.height, 0, \none);

			vlines = vlines.explin( min, max, 0, bounds.width );
			dimvlines = dimvlines.explin( min, max, 0, bounds.width );

			pt = params.collect({ |array|
				(array[0].explin( min, max, 0, bounds.width ))
				@
				(array[1].linlin(range.neg,range,bounds.height,0,\none));
			});

			Pen.color_( Color.white.alpha_(0.25) );
			Pen.roundedRect( bounds, [6,6,0,0] ).fill;

			Pen.color = Color.gray(0.2).alpha_(0.5);
			//Pen.strokeRect( bounds.insetBy(-1,-1) );

			//Pen.addRect( bounds ).clip;
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

			/*
			Pen.color = Color.white.alpha_(0.5);
			Pen.fillRect( Rect( 33, 0, 206, 14 ) );
			*/

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

			//Pen.roundedRect( bounds.insetBy(0.5,0.5), [5,5,0,0] ).stroke;

			/*
			if( selected != -1 )
			{ Pen.stringAtPoint(
			[ "low shelf: %hz, %dB, rs=%",
			"peak 1: %hz, %dB, rq=%",
			"peak 2: %hz, %dB, rq=%",
			"peak 3: %hz, %dB, rq=%",
			"hi shelf: %hz, %dB, rs=%"
			][ selected ].format(
			params[selected][0],
			params[selected][1],
			params[selected][2]
			),
			35@1 );
			}
			{ Pen.stringAtPoint( "shift: snap, alt: rq", 35@1 ); };
			*/

			if( selected != -1, {
				var string, strBounds;

				string = [ "low shelf: %hz, %dB, rs=%",
						"peak 1: %hz, %dB, rq=%",
						"peak 2: %hz, %dB, rq=%",
						"peak 3: %hz, %dB, rq=%",
						"hi shelf: %hz, %dB, rs=%"
					][ selected ].format(
						params[selected][0],
						params[selected][1],
						params[selected][2]
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
		//eq[ \pu_filebuttons ][1].action.value; // revert
		this.refresh;

		//this.refreshInRect( this.bounds.insetBy(-2,-2) );

		this.mouseDownAction = { |vw,x,y,mod|
			var bounds;
			var pt;
			var min = 20, max = 22050, range = 24;

			bounds = vw.bounds.moveTo(0,0);
			//pt = (x@y) - (bounds.leftTop);
			pt = (x@y);

			selected =  params.detectIndex({ |array|
				(( array[ 0 ].explin( min, max, 0, bounds.width ) )@
					( array[ 1 ].linlin( range.neg, range, bounds.height, 0, \none ) ))
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
			//pt = (x@y) - (bounds.leftTop);
			pt = (x@y);

			if( selected != -1 )
			{
				case { mod.isAlt }
				{
					if(  ModKey( mod ).shift )
					{
						params[selected] = params[selected][[0,1]]
						++ [ y.linexp( bounds.height, 0, 0.1, 10, \none ).nearestInList(
							if( [0,4].includes(selected) )
							{[0.6,1,2.5,5,10]}
							{[0.1,0.25,0.5,1,2.5,5,10]}

						) ];
					}
					{
						params[selected] = params[selected][[0,1]]
						++ [ (y - downY + params[selected][2].explin( 0.1, 10, bounds.height, 0, \none )).linexp( bounds.height, 0, 0.1, 10, \none ).clip(
							if( [0,4].includes(selected) ) { 0.6 } {0.1},
							10).round(0.01)];
					};
				}
				{ ModKey( mod ).shift }
				{
					params[selected] = [
						pt.x.linexp(0, bounds.width, min, max )
						.nearestInList( [25,50,75,100,250,500,750,1000,2500,5000,7500,10000] ),
						pt.y.linlin( 0, bounds.height, range, range.neg, \none )
						.clip2( range ).round(6),
						params[selected][2]
					];
				}
				{ true }
				{
					params[selected] = [
						pt.x.linexp(0, bounds.width, min, max ).clip(20,20000).round(1),
						pt.y.linlin( 0, bounds.height, range, range.neg, \none ).clip2( range )
						.round(0.25),
						params[selected][2]
					];
					vw.refresh;
				};
				this.doAction;
				vw.refresh;

			};


		}
	}

	doAction {
		action.value(params);
		target.set(
			\loShelf, params[0][[0, 2, 1]], \loPeak, params[1][[0, 2, 1]], \midPeak, params[2][[0, 2, 1]], \hiPeak, params[3][[0, 2, 1]], \hiShelf, params[4][[0, 2, 1]])
	}
}

+ UGen {

	equise {
		var params, chain;
		params = [[100,0,1], [250,0,1], [1000,0,1], [3500,0,1], [6000,0,1]]; // defaults
		chain = this;

		chain = BLowShelf.ar( chain, *LagControl.names([\loShelf]).kr(params[0][[0,2,1]], 0.1));
		chain = BPeakEQ.ar( chain, *LagControl.names([\loPeak]).kr(params[1][[0,2,1]], 0.1));
		chain = BPeakEQ.ar( chain, *LagControl.names([\midPeak]).kr(params[2][[0,2,1]], 0.1));
		chain = BPeakEQ.ar( chain, *LagControl.names([\hiPeak]).kr(params[3][[0,2,1]], 0.1));
		chain = BHiShelf.ar( chain, *LagControl.names([\hiShelf]).kr(params[4][[0,2,1]], 0.1));
		chain = RemoveBadValues.ar( chain );

		^chain;
	}
}